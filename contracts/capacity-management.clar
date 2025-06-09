;; Capacity Management Contract
;; Optimizes storage utilization and manages capacity allocation

(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_INSUFFICIENT_CAPACITY (err u201))
(define-constant ERR_INVALID_ALLOCATION (err u202))
(define-constant ERR_FACILITY_NOT_VERIFIED (err u203))

(define-map capacity-allocations
  { facility-id: uint, allocation-id: uint }
  {
    allocated-capacity: uint,
    start-time: uint,
    end-time: uint,
    purpose: (string-ascii 50),
    allocated-by: principal
  }
)

(define-map facility-utilization
  { facility-id: uint }
  {
    total-capacity: uint,
    allocated-capacity: uint,
    available-capacity: uint,
    efficiency-rating: uint
  }
)

(define-data-var next-allocation-id uint u1)

(define-public (allocate-capacity (facility-id uint) (capacity uint) (duration uint) (purpose (string-ascii 50)))
  (let (
    (allocation-id (var-get next-allocation-id))
    (utilization (default-to
      { total-capacity: u0, allocated-capacity: u0, available-capacity: u0, efficiency-rating: u100 }
      (map-get? facility-utilization { facility-id: facility-id })
    ))
  )
    (asserts! (>= (get available-capacity utilization) capacity) ERR_INSUFFICIENT_CAPACITY)
    (asserts! (> capacity u0) ERR_INVALID_ALLOCATION)

    (map-set capacity-allocations
      { facility-id: facility-id, allocation-id: allocation-id }
      {
        allocated-capacity: capacity,
        start-time: block-height,
        end-time: (+ block-height duration),
        purpose: purpose,
        allocated-by: tx-sender
      }
    )

    (map-set facility-utilization
      { facility-id: facility-id }
      (merge utilization {
        allocated-capacity: (+ (get allocated-capacity utilization) capacity),
        available-capacity: (- (get available-capacity utilization) capacity)
      })
    )

    (var-set next-allocation-id (+ allocation-id u1))
    (ok allocation-id)
  )
)

(define-public (release-capacity (facility-id uint) (allocation-id uint))
  (let (
    (allocation (unwrap! (map-get? capacity-allocations { facility-id: facility-id, allocation-id: allocation-id }) ERR_INVALID_ALLOCATION))
    (utilization (unwrap! (map-get? facility-utilization { facility-id: facility-id }) ERR_INVALID_ALLOCATION))
  )
    (asserts! (is-eq tx-sender (get allocated-by allocation)) ERR_UNAUTHORIZED)

    (map-delete capacity-allocations { facility-id: facility-id, allocation-id: allocation-id })

    (map-set facility-utilization
      { facility-id: facility-id }
      (merge utilization {
        allocated-capacity: (- (get allocated-capacity utilization) (get allocated-capacity allocation)),
        available-capacity: (+ (get available-capacity utilization) (get allocated-capacity allocation))
      })
    )
    (ok true)
  )
)

(define-public (update-facility-capacity (facility-id uint) (total-capacity uint))
  (let ((current-utilization (default-to
    { total-capacity: u0, allocated-capacity: u0, available-capacity: u0, efficiency-rating: u100 }
    (map-get? facility-utilization { facility-id: facility-id })
  )))
    (map-set facility-utilization
      { facility-id: facility-id }
      (merge current-utilization {
        total-capacity: total-capacity,
        available-capacity: (- total-capacity (get allocated-capacity current-utilization))
      })
    )
    (ok true)
  )
)

(define-read-only (get-facility-utilization (facility-id uint))
  (map-get? facility-utilization { facility-id: facility-id })
)

(define-read-only (get-allocation (facility-id uint) (allocation-id uint))
  (map-get? capacity-allocations { facility-id: facility-id, allocation-id: allocation-id })
)
