;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TUIO Client Demo in Universe.ss (2htdp/universe & 2htdp/image)             ;;
;; - part of scheme-tuio-0.2                                                  ;;
;; Copyright (c) 2009-2010 Eren Güven <erenguven@cs.bilgi.edu.tr>             ;;
;;                                                                            ;;
;; This program is free software: you can redistribute it and/or modify       ;;
;; it under the terms of the GNU General Public License as published by       ;;
;; the Free Software Foundation, either version 3 of the License, or          ;;
;; (at your option) any later version.                                        ;;
;;                                                                            ;;
;; This program is distributed in the hope that it will be useful,            ;;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of             ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              ;;
;; GNU General Public License for more details.                               ;;
;;                                                                            ;;
;; You should have received a copy of the GNU General Public License          ;;
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang scheme
(require 2htdp/universe
         2htdp/image
         "SchemeTuio.ss")

; world: list-of-2Dcur list-of-2Dobj
(define-struct world (cursor-list object-list))
; black circle for cursor, red square for object
(define CUR (circle 10 'solid 'black))
(define OBJ (rectangle 30 30 'solid 'red))
; screen size
(define S_WID 800)
(define S_HEI 600)

; radian to degree convertion (TUIO uses radians, 2htdp/image.ss uses degrees)
(define (radian->degree ang)
  (/ (* ang 180) pi))

; function to call on tick-event
; - update cursor and object lists in world
; note that these two functions can be used anywhere once the client is started
(define (iTick w)
  (make-world (get-tuio-cursors)(get-tuio-objects)))

; function to call on draw
; - send both lists to helper function for drawing
(define (iDraw w)
  (draw-helper (world-cursor-list w) (world-object-list w)))

; draws all cursors(+ path) and calls helper function with object-list
(define (draw-helper l-cur l-obj)
  (cond
    [(empty? l-cur) (draw-helper-o l-obj)] ; draw objects once cursors are done
    ; draw cursor
    [else (place-image CUR (* S_WID (2Dcur-posX (first l-cur))) (* S_HEI (2Dcur-posY (first l-cur)))
                       ; draw path
                       (place-image (path-drawer (2Dcur-path (first l-cur))) (/ S_WID 2) (/ S_HEI 2)                      
                       (draw-helper (rest l-cur) l-obj)))] ))

; draws all objects(+ path)
; note that rotate is mirrored
(define (draw-helper-o l-obj)
  (cond
    [(empty? l-obj) (rectangle S_WID S_HEI 'solid 'white)]
    [else (place-image (text (number->string (2Dobj-symID (first l-obj))) 12 "indigo") ; insert object's symbol id
                       (- (* S_WID (2Dobj-posX (first l-obj))) 5) ; position for symID
                       (+ (* S_HEI (2Dobj-posY (first l-obj))) 5)
                       ; draw object
                       (place-image (rotate (* -1 (radian->degree (2Dobj-angle (first l-obj)))) OBJ) ; rotate obj
                                    (* S_WID (2Dobj-posX (first l-obj))) (* S_HEI (2Dobj-posY (first l-obj)))
                                    (draw-helper-o (rest l-obj))))] ))

; draws a given path (list-of-TuioPoint)
; optional
; note that drawing paths requires extra processing power thus slowing the application
; (once the paths get longer and/or there are many active cursors/objects )
; can be used both on 2Dcur and 2Dobj paths
(define (path-drawer a-path)
  (cond
    [(< (length a-path) 2) (rectangle S_WID S_HEI 'outline 'black)]
    [(= (length a-path) 2) 
     (scene+line (rectangle S_WID S_HEI 'outline 'black)
                 (* S_WID (TuioPoint-posX (first a-path))) (* S_HEI (TuioPoint-posY (first a-path)))
                 (* S_WID (TuioPoint-posX (second a-path))) (* S_HEI (TuioPoint-posY (second a-path))) 'gray)]
  [else
   (scene+line (path-drawer (rest a-path))
               (* S_WID (TuioPoint-posX (first a-path))) (* S_HEI (TuioPoint-posY (first a-path)))
               (* S_WID (TuioPoint-posX (second a-path))) (* S_HEI (TuioPoint-posY (second a-path))) 'gray)] ))


; IMPORTANT
; Tuio Client needs to be started before big-bang
; Tuio Client Thread needs to be killed manually (Ctrl-K in PLT-Scheme)
; or you can use the simple trick below

; start client this way
(define client-thread (start-tuio-client "127.0.0.1" 3333))

; then enter your big-bang call
(big-bang (make-world (get-tuio-cursors)(get-tuio-objects)) ; initial world
          (on-tick iTick) ; tick
          (on-draw iDraw)) ; draw

; once big-bang is stopped, below function will be called, thus terminating the thread
(kill-thread client-thread)

; the clock can be set to tick at higher rate (see big-bang documentation in 2htdp/universe) than standart
; for faster computers and/or where tracking is handled on another computer
