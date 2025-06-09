;; Grid Services Contract
;; Provides system support services for grid stability

(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_INVALID_SERVICE (err u301))
(define-constant ERR_SERVICE_NOT_FOUND (err u302))
(define-constant ERR_INSUFFICIENT_CAPACITY (err u303))

(define-map grid-services
  { service-id: uint }
  {
    facility-id: uint,
    service-type: (string-ascii 30),
    capacity-provided: uint,
    frequency-regulation: bool,
    voltage-support: bool,
    start-time: uint,
    duration: uint,
    compensation-rate: uint,
    provider: principal
  }
)

(define-map service-requests
  { request-id: uint }
  {
    service-type: (string-ascii 30),
    required-capacity: uint,
    duration: uint,
    compensation-offered: uint,
    requester: principal,
    fulfilled: bool
  }
)

(define-data-var next-service-id uint u1)
(define-data-var next-request-id uint u1)

(define-public (provide-grid-service
  (facility-id uint)
  (service-type (string-ascii 30))
  (capacity uint)
  (frequency-regulation bool)
  (voltage-support bool)
  (duration uint)
  (compensation-rate uint)
)
  (let ((service-id (var-get next-service-id)))
    (asserts! (> capacity u0) ERR_INVALID_SERVICE)
    (asserts! (> duration u0) ERR_INVALID_SERVICE)

    (map-set grid-services
      { service-id: service-id }
      {
        facility-id: facility-id,
        service-type: service-type,
        capacity-provided: capacity,
        frequency-regulation: frequency-regulation,
        voltage-support: voltage-support,
        start-time: block-height,
        duration: duration,
        compensation-rate: compensation-rate,
        provider: tx-sender
      }
    )

    (var-set next-service-id (+ service-id u1))
    (ok service-id)
  )
)

(define-public (request-grid-service
  (service-type (string-ascii 30))
  (required-capacity uint)
  (duration uint)
  (compensation-offered uint)
)
  (let ((request-id (var-get next-request-id)))
    (asserts! (> required-capacity u0) ERR_INVALID_SERVICE)
    (asserts! (> duration u0) ERR_INVALID_SERVICE)

    (map-set service-requests
      { request-id: request-id }
      {
        service-type: service-type,
        required-capacity: required-capacity,
        duration: duration,
        compensation-offered: compensation-offered,
        requester: tx-sender,
        fulfilled: false
      }
    )

    (var-set next-request-id (+ request-id u1))
    (ok request-id)
  )
)

(define-public (fulfill-service-request (request-id uint) (service-id uint))
  (let (
    (request (unwrap! (map-get? service-requests { request-id: request-id }) ERR_SERVICE_NOT_FOUND))
    (service (unwrap! (map-get? grid-services { service-id: service-id }) ERR_SERVICE_NOT_FOUND))
  )
    (asserts! (>= (get capacity-provided service) (get required-capacity request)) ERR_INSUFFICIENT_CAPACITY)
    (asserts! (is-eq (get service-type service) (get service-type request)) ERR_INVALID_SERVICE)

    (map-set service-requests
      { request-id: request-id }
      (merge request { fulfilled: true })
    )
    (ok true)
  )
)

(define-read-only (get-grid-service (service-id uint))
  (map-get? grid-services { service-id: service-id })
)

(define-read-only (get-service-request (request-id uint))
  (map-get? service-requests { request-id: request-id })
)
