;; identity-verification.clar
;; This contract validates the identity of underserved individuals

(define-data-var admin principal tx-sender)

;; Data structure for identity records
(define-map identities
  { user: principal }
  {
    verified: bool,
    verification-date: uint,
    verification-level: uint,
    verification-expiry: uint
  }
)

;; Events for tracking identity verification
(define-public (verify-identity (verification-level uint) (expiry uint))
  (let
    (
      (user tx-sender)
      (current-time (get-block-info? time (- block-height u1)))
    )
    (asserts! (is-some current-time) (err u1))
    (map-set identities
      { user: user }
      {
        verified: true,
        verification-date: (unwrap-panic current-time),
        verification-level: verification-level,
        verification-expiry: (+ (unwrap-panic current-time) expiry)
      }
    )
    (ok true)
  )
)

;; Check if a user is verified
(define-read-only (is-verified (user principal))
  (let
    (
      (identity-data (map-get? identities { user: user }))
      (current-time (get-block-info? time (- block-height u1)))
    )
    (if (and
          (is-some identity-data)
          (is-some current-time)
          (get verified (unwrap-panic identity-data))
          (< (unwrap-panic current-time) (get verification-expiry (unwrap-panic identity-data)))
        )
      true
      false
    )
  )
)

;; Get verification level of a user
(define-read-only (get-verification-level (user principal))
  (let
    (
      (identity-data (map-get? identities { user: user }))
    )
    (if (is-some identity-data)
      (ok (get verification-level (unwrap-panic identity-data)))
      (err u1)
    )
  )
)

;; Admin function to revoke verification
(define-public (revoke-verification (user principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u403))
    (map-delete identities { user: user })
    (ok true)
  )
)

;; Transfer admin rights
(define-public (transfer-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u403))
    (var-set admin new-admin)
    (ok true)
  )
)
