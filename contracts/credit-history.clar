;; credit-history.clar
;; This contract records alternative financial data for credit history

(define-data-var admin principal tx-sender)

;; Data structure for credit records
(define-map credit-records
  { user: principal }
  {
    payment-count: uint,
    on-time-payments: uint,
    late-payments: uint,
    total-borrowed: uint,
    total-repaid: uint,
    last-updated: uint
  }
)

;; Initialize a credit record
(define-public (initialize-credit-record)
  (let
    (
      (user tx-sender)
      (current-time (get-block-info? time (- block-height u1)))
    )
    (asserts! (is-some current-time) (err u1))
    (map-set credit-records
      { user: user }
      {
        payment-count: u0,
        on-time-payments: u0,
        late-payments: u0,
        total-borrowed: u0,
        total-repaid: u0,
        last-updated: (unwrap-panic current-time)
      }
    )
    (ok true)
  )
)

;; Record a new loan
(define-public (record-loan (user principal) (amount uint))
  (let
    (
      (current-record (map-get? credit-records { user: user }))
      (current-time (get-block-info? time (- block-height u1)))
    )
    (asserts! (is-eq tx-sender (var-get admin)) (err u403))
    (asserts! (is-some current-record) (err u404))
    (asserts! (is-some current-time) (err u1))

    (map-set credit-records
      { user: user }
      {
        payment-count: (get payment-count (unwrap-panic current-record)),
        on-time-payments: (get on-time-payments (unwrap-panic current-record)),
        late-payments: (get late-payments (unwrap-panic current-record)),
        total-borrowed: (+ (get total-borrowed (unwrap-panic current-record)) amount),
        total-repaid: (get total-repaid (unwrap-panic current-record)),
        last-updated: (unwrap-panic current-time)
      }
    )
    (ok true)
  )
)

;; Record a payment
(define-public (record-payment (user principal) (amount uint) (on-time bool))
  (let
    (
      (current-record (map-get? credit-records { user: user }))
      (current-time (get-block-info? time (- block-height u1)))
    )
    (asserts! (is-eq tx-sender (var-get admin)) (err u403))
    (asserts! (is-some current-record) (err u404))
    (asserts! (is-some current-time) (err u1))

    (map-set credit-records
      { user: user }
      {
        payment-count: (+ (get payment-count (unwrap-panic current-record)) u1),
        on-time-payments: (+ (get on-time-payments (unwrap-panic current-record)) (if on-time u1 u0)),
        late-payments: (+ (get late-payments (unwrap-panic current-record)) (if on-time u0 u1)),
        total-borrowed: (get total-borrowed (unwrap-panic current-record)),
        total-repaid: (+ (get total-repaid (unwrap-panic current-record)) amount),
        last-updated: (unwrap-panic current-time)
      }
    )
    (ok true)
  )
)

;; Get credit score (simple calculation)
(define-read-only (get-credit-score (user principal))
  (let
    (
      (record (map-get? credit-records { user: user }))
    )
    (if (is-some record)
      (let
        (
          (unwrapped-record (unwrap-panic record))
          (payment-count (get payment-count unwrapped-record))
          (on-time-ratio (if (> payment-count u0)
                           (/ (* (get on-time-payments unwrapped-record) u100) payment-count)
                           u0))
          (repayment-ratio (if (> (get total-borrowed unwrapped-record) u0)
                             (/ (* (get total-repaid unwrapped-record) u100) (get total-borrowed unwrapped-record))
                             u0))
        )
        ;; Simple score calculation: 50% on-time ratio + 50% repayment ratio
        (ok (/ (+ on-time-ratio repayment-ratio) u2))
      )
      (err u404)
    )
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
