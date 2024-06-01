;;                    <Autors>
;; Carlos Lozano Alemañy / Michael Javier Cataño Ortiz

;; ------ MÈTODE inicia ------
;; Estableix les propietats necessàries per representar l’escenari i els seus elements. 
(defun inicia ()
;; Definir dimensions de la superfície
(putprop 'superficie 640 'ample) (putprop 'superficie 340 'alt)
;; Definir dimensions del mur
(putprop 'mur (funcall 'rnd 20 40) 'b) (putprop 'mur (funcall 'rnd 100 150) 'e) 
;; Definir la diferència aleatòria entre ambdós camps, que serà d'entre 0 i 40 px
(putprop 'camps (funcall 'rnd 0 40) 'dif)
;; Definir dimensions per cada camp
(putprop 'camp1 (funcall 'amplada_camp_1) 'a) (putprop 'camp1 (funcall 'rnd 15 45) 'd) 
(putprop 'camp2 (funcall 'amplada_camp_2) 'c) (putprop 'camp2 (funcall 'rnd 15 45) 'f) 
;; Propietats de la base de cada canó
(putprop 'base_cano1 20 'ample) (putprop 'base_cano1 10 'altura)
(putprop 'base_cano1 (round (funcall 'rnd (/ (get 'camp1 'a) 3) (* 2 (/ (get 'camp1 'a) 3)))) 'terc_central) ;; Terç central
(putprop 'base_cano2 20 'ample) (putprop 'base_cano2 10 'altura)
(putprop 'base_cano2 (round (funcall 'rnd (/ (get 'camp2 'c) 3) (* 2 (/ (get 'camp2 'c) 3)))) 'terc_central) ;; Terç central
(putprop 'base_cano2 (funcall 'pos_x_base_cano2 ) 'pos_x) ;; Calcular quina es la posició eix X del terç central obtingut
;; Propietats de cada canó
(putprop 'cano1 45 'angle) (putprop 'cano1 55 'v_inicial)  
(putprop 'cano1 (+ (get 'base_cano1 'terc_central) (/ (get 'base_cano1 'ample) 2)) 'x) ;; pos x inicial del canó
(putprop 'cano1 (+ (get 'camp1 'd) (get 'base_cano1 'altura)) 'y) ;; pos y inicial del canó
(putprop 'cano1 1 'vida)
(putprop 'cano2 135 'angle) (putprop 'cano2 55 'v_inicial)
(putprop 'cano2 (+ (get 'base_cano2 'pos_x) (/ (get 'base_cano2 'ample) 2)) 'x) ;; pos x inicial del canó
(putprop 'cano2 (+ (get 'camp2 'f) (get 'base_cano2 'altura)) 'y) ;; pos y inicial del canó
(putprop 'cano2 1 'vida)
;; Propietats del pal
(putprop 'pal 20 'altura)
;; Propietats de la bandera
(putprop 'bandera (funcall 'rnd -5 5) 'forza_vent)
(putprop 'bandera 10 'escala) ;; la bandera es dibuixarà a una escala de 1:10
(putprop 'bandera 20 'altura) 
(putprop 'bandera (+ (get 'camp1 'a) (round (/ (get 'mur 'b) 2))) 'pos_central_x) ;; Punt central de la bandera
(putprop 'bandera (- (get 'bandera 'pos_central_x) (abs ( * (get 'bandera 'forza_vent) (get 'bandera 'escala)))) 'pos_esq_x)
;; Trajectòria
(putprop 'traj -9.8 'g) ;; acceleració constant (gravetat)

(bucle))

;; ------ MÈTODE pinta ------
;; Esborra  la  pantalla  i  torna  a  dibuixar  l’escenari  amb  tots  els  seus 
;; elements segons les seves propietats.
(defun pinta ()
(cls) 
(pintar_escenari);; Escenari inicial
(color 0 145 63 0 0 0)
(pintar_camp1) ;; Camp esquerre
(pintar_camp2) ;; Camp dret
(color 0 0 0 255 255 255)
(pintar_mur) ;; Mur
(pintar_base_cano1) ;; Base canó 1
(pintar_base_cano2) ;; Base canó 2
(pintar_cano1) ;; Canó 1
(pintar_cano2) ;; Canó 2
(color 180 0 0 0 0 0)
(pintar_pal) ;; Pal
(pintar_bandera_final) ;; Bandera (escala 1:10)
(color 0 0 0 255 255 255)
)

;; ------ MÈTODE bucle ------
;; Obté  l’entrada  de  l’usuari,  interpreta  l’opció, du  a  terme  l’acció desitjada, repinta l’escenari i torna a començar
;; Acaba en introduir una tecla específica o quan s’ha destruït un canó.
(defun bucle ()
(pinta)
;; Si els canons no tenen vides sortim del bucle i mostram un missatge per pantalla
(cond ((or (= (get 'cano1 'vida) 0)(= (get 'cano2 'vida) 0))
(format t "FI DEL JOC !!      Vides:       Cano Esquerra ~a     Cano Dret ~a~%" (get 'cano1 'vida) (get 'cano2 'vida)))
;; Si encara tenen vides continuam amb el bucle
(t


(cond
    ((equal (get-key) 97) (moure_cano1_esq ))   ; ASCII per 'a'
    ((equal (get-key) 106) (moure_cano2_esq ))  ; ASCII per 'j'
    ((equal (get-key) 100) (moure_cano1_dreta ))  ; ASCII per 'd'
    ((equal (get-key) 108) (moure_cano2_dreta ))  ; ASCII per 'l'
    ((equal (get-key) 119) (pujar_cano1 ))  ; ASCII per 'w'
    ((equal (get-key) 105) (pujar_cano2 ))  ; ASCII per 'i'
    ((equal (get-key) 115) (baixar_cano1 ))  ; ASCII per 's'
    ((equal (get-key) 107) (baixar_cano2 ))  ; ASCII per 'k'
    ((equal (get-key) 113) (menys_pot_cano1 ))  ; ASCII per 'q'
    ((equal (get-key) 111) (menys_pot_cano2 ))  ; ASCII per 'o'
    ((equal (get-key) 101) (mes_pot_cano1 ))  ; ASCII per 'e'
    ((equal (get-key) 117) (mes_pot_cano2 ))  ; ASCII per 'u'
    ((equal (get-key) 102) (dispara_cano1 ))  ; ASCII per 'f'
    ((equal (get-key) 104) (dispara_cano2 ))  ; ASCII per 'h'
    ((equal (get-key) 27) t)  ; ASCII per 'ESC' (Acaba recursió)
    (t (bucle)))))) ;; altrament

;; ------ MÈTODE per moure els canons cap a la esquerra o dreta ------
(defun moure_cano1_esq ()
  (cond
    ((>  (get 'base_cano1 'terc_central) 0) 
    (putprop 'base_cano1 (- (get 'base_cano1 'terc_central) 1) 'terc_central)
    (putprop 'cano1 (- (get 'cano1 'x) 1) 'x)) ;; ara la pos x del cano 1 es troba -1 a la esquerra
    (t t)
  )
(bucle))

(defun moure_cano2_esq ()
  (cond
    ((>(get 'base_cano2 'pos_x) (+(get 'camp1 'a)(get 'mur 'b))) (putprop 'base_cano2 (- (get 'base_cano2 'pos_x) 1) 'pos_x)
    (putprop 'cano2 (- (get 'cano2 'x) 1) 'x)) ;; ara la pos x del cano 2 es troba -1 a la esquerra
    (t t)
  )
(bucle))

(defun moure_cano1_dreta ()
  (cond
    ((< (get 'base_cano1 'terc_central)(- (get 'superficie 'ample) (+(get 'camp2 'c)(get 'mur 'b)(get 'base_cano1 'ample)))) 
    (putprop 'base_cano1 (+ (get 'base_cano1 'terc_central) 1) 'terc_central)
    (putprop 'cano1 (+ (get 'cano1 'x) 1) 'x)) ;; ara la pos x del cano 1 es troba +1 a la dreta
    (t t)
  )
(bucle))

(defun moure_cano2_dreta ()
  (cond
    ((< (get 'base_cano2 'pos_x)(-(get 'superficie 'ample)(get 'base_cano2 'ample))) (putprop 'base_cano2 (+ (get 'base_cano2 'pos_x) 1) 'pos_x)
    (putprop 'cano2 (+ (get 'cano2 'x) 1) 'x)) ;; ara la pos x del cano 2 es troba +1 a la dreta
    (t t)
  )
(bucle))

;; ------ MÈTODES pujar/baixar els graus d'orientació del canó ------ 
(defun pujar_cano1 ()
  (cond
    ((< (get 'cano1 'angle) 150) 
    (putprop 'cano1 (+ (get 'cano1 'angle) 5) 'angle))
    (t t)
  )
(bucle)
)

(defun pujar_cano2 () ;; pujar canó 2 implica disminuir els graus
  (cond
    ((> (get 'cano2 'angle) 30) (putprop 'cano2 (- (get 'cano2 'angle) 5) 'angle))
    (t t)
  )
(bucle)
)

(defun baixar_cano1 () ;; baixar canó 1 implica disminuir els graus 
  (cond
    ((> (get 'cano1 'angle) 30) (putprop 'cano1 (- (get 'cano1 'angle) 5) 'angle))
    (t t)
  )
(bucle)
)

(defun baixar_cano2 () ;; baixar canó 2 implica augmentar els graus
  (cond
    ((< (get 'cano2 'angle) 150) (putprop 'cano2 (+ (get 'cano2 'angle) 5) 'angle))
    (t t)
  )
(bucle)
)

;; ------ MÈTODES augmentar/disminuir la potència del canó ------
(defun mes_pot_cano1 ()
(putprop 'cano1 (+ (get 'cano1 'v_inicial) 2) 'v_inicial) ;; cada vegada aumentaría 1 ja que la escala és de 1:2
(bucle)
)

(defun mes_pot_cano2 ()
(putprop 'cano2 (+ (get 'cano2 'v_inicial) 2) 'v_inicial) ;; cada vegada aumentaría 1 ja que la escala és de 1:2
(bucle)
)

(defun menys_pot_cano1 ()
  (cond
  ;; cada vegada disminuiria 1 ja que la escala és de 1:2
    ((> (get 'cano1 'v_inicial) 0) (putprop 'cano1 (- (get 'cano1 'v_inicial) 2) 'v_inicial))
    (t t)
  )
(bucle)
)

(defun menys_pot_cano2 ()
  (cond
  ;; cada vegada disminuiria 1 ja que la escala és de 1:2
    ((> (get 'cano2 'v_inicial) 0) (putprop 'cano2 (- (get 'cano2 'v_inicial) 2) 'v_inicial))
    (t t)
  )
(bucle) ;; tornam al bucle per triar la següent acció
)

;; ------ MÈTODES dispara ------
;; Donat  un  canó,  calcula  i  dibuixa  la  trajectòria  del  projectil  disparat, 
;; segons la posició del canó, l’angle del dispar i la seva velocitat inicial. 
;; El càlcul s’atura quan el projectil pega a terra, al mur del mig, a un canó o si surt per un lateral.

(defun dispara_cano1 ()
(pintar_cano1) ;; representam el cano 1
(definir_valors_cano1) ;; Definim els valors per calcular la trajectoria
(color 234 190 63 0 0 0 )
(pintar_trajectoria_cano1) ;; pintam la trajectòria fins que aquesta toqui un element
(color 0 0 0 255 255 255)
(bucle) ;; tornam al bucle per triar la següent acció
)

(defun dispara_cano2 ()
(pintar_cano2) ;; representam el cano 2
(definir_valors_cano2) ;; Definim els valors per calcular la trajectoria
(color 234 190 63 0 0 0 )
(pintar_trajectoria_cano2) ;; pintam la trajectòria fins que aquesta toqui un element
(color 0 0 0 255 255 255)
(bucle) ;; tornam al bucle per triar la següent acció
)

;; ------ FUNCIONS PER LA TRAJECTÒRIA ------

;; Com al bucle "pintar_trajectoria_cano1" calculam les posicions (x i y; vx i vy) en 
;; funció de les anteriors, necessitam restablir els valors per quan tornem a calcular-la
(defun definir_valors_cano1 ()
(putprop 'cano1 (+ (get 'base_cano1 'terc_central) (/ (get 'base_cano1 'ample) 2)) 'x) ;; pos x inicial del canó
(putprop 'cano1 (+ (get 'camp1 'd) (get 'base_cano1 'altura)) 'y) ;; pos y inicial del canó
(putprop 'cano1 (x_final_cano1 (get 'cano1 'x)) 'x) ;; determinam els punts x,y de la punta del canó
(putprop 'cano1 (y_final_cano1 (get 'cano1 'y)) 'y)
(putprop 'traj_cano1 (* (get 'cano1 'v_inicial) (cos (radians (get 'cano1 'angle)))) 'v_ix) ;; obtenim la velocitat inicial
(putprop 'traj_cano1 (* (get 'cano1 'v_inicial) (sin (radians (get 'cano1 'angle)))) 'v_iy)
(putprop 'traj 0 'dt) ) ;; instant inicial = 0

(defun definir_valors_cano2 ()
(putprop 'cano2 (+ (get 'base_cano2 'pos_x) (/ (get 'base_cano2 'ample) 2)) 'x) ;; pos x inicial del canó
(putprop 'cano2 (+ (get 'camp2 'f) (get 'base_cano2 'altura)) 'y) ;; pos y inicial del canó
(putprop 'cano2 (x_final_cano2 (get 'cano2 'x)) 'x) ;; determinam els punts x,y de la punta del canó
(putprop 'cano2 (y_final_cano2 (get 'cano2 'y)) 'y)
(putprop 'traj_cano2 (* (get 'cano2 'v_inicial) (cos (radians (get 'cano2 'angle)))) 'v_ix) ;; obtenim la velocitat inicial
(putprop 'traj_cano2 (* (get 'cano2 'v_inicial) (sin (radians (get 'cano2 'angle)))) 'v_iy)
(putprop 'traj 0 'dt) ) ;; reiniciam el temps


;; càlcul de les velocitats vx , vy de cada canó; 
;; ambdós depenen de la velocitat del instant anterior i del temps
(defun calcular_vx_1 (v0_x_1 dt)
  (+ v0_x_1 (* (get 'bandera 'forza_vent) dt)))

(defun calcular_vx_2 (v0_x_2 dt)
  (+ v0_x_2 (* (get 'bandera 'forza_vent) dt)))

(defun calcular_vy_1 (v0_y_1 dt)
  (+ v0_y_1 (* (get 'traj 'g) dt)))

(defun calcular_vy_2 (v0_y_2 dt)
  (+ v0_y_2 (* (get 'traj 'g) dt)))

;; càlcul de les posicions px , py de la bala de cada canó; 
;; ambdós depenen de la posició del instant anterior, de la velocitat del instant anterior i del temps
(defun calcular_px_1 (x0_1 vx_1 dt)
  (+ x0_1 (* vx_1 dt)))

(defun calcular_px_2 (x0_2 vx_2 dt)
  (+ x0_2 (* vx_2 dt)))

(defun calcular_py_1 (y0_1 vy_1 dt)
  (+ y0_1 (* vy_1 dt) (* 1/2 (get 'traj 'g) (expt dt 2))))

(defun calcular_py_2 (y0_2 vy_2 dt)
  (+ y0_2 (* vy_2 dt) (* 1/2 (get 'traj 'g) (expt dt 2))))

;; càlcul de la posicio x,y de la punta del canó en funció del punt inicial del canó
;; ens servirà per disparar la bala a partir de la punta del canó
(defun x_final_cano1 (x)
    (+ x (* (round (/ (get 'cano1 'v_inicial) 2)) (cos (radians (get 'cano1 'angle))))))

(defun y_final_cano1 (y)
    (+ y (* (round (/ (get 'cano1 'v_inicial) 2)) (sin (radians (get 'cano1 'angle))))))

(defun x_final_cano2 (x)
    (+ x (* (round (/ (get 'cano2 'v_inicial) 2)) (cos (radians (get 'cano2 'angle))))))

(defun y_final_cano2 (y)
    (+ y (* (round (/ (get 'cano2 'v_inicial) 2)) (sin (radians (get 'cano2 'angle))))))

(defun pintar_trajectoria_cano1 ()
  (let ((x (get 'cano1 'x))(y (get 'cano1 'y)))
    (if (punt_toca_cano2 x y)
        (progn
          ;; Si toca el canó 2, li restam 1 vida i sortim del bucle
          (putprop 'cano1 (- (get 'cano1 'vida) 1) 'vida)
          ;; Mostrar mensaje per pantalla
          (format t "                   Vides:       Cano Esquerra ~a     Cano Dret ~a~%" (get 'cano1 'vida) (get 'cano2 'vida)))
      ;; Si el punt no toca àrea aleshores pintam 
      (unless (punt_toca_area x y)
        (drawr x y)
        (calc_proxim_punt_cano1) ;; Funció per calcular el següent punt
        (sleep 0.075) ;; sleep per que es vegi millor 
        (pintar_trajectoria_cano1))))) ;; tornam al bucle

(defun pintar_trajectoria_cano2 ()
  (let ((x (get 'cano2 'x))(y (get 'cano2 'y)))
    (if (punt_toca_cano1 x y)
        (progn
          ;; Si toca el canó 1, li restam 1 vida i sortim del bucle
          (putprop 'cano2 (- (get 'cano2 'vida) 1) 'vida)
          ;; Mostrar mensaje per pantalla
          (format t "                   Vides:       Cano Esquerra ~a     Cano Dret ~a~%" (get 'cano1 'vida) (get 'cano2 'vida)))
      ;; Si el punt no toca àrea aleshores pintam 
      (unless (punt_toca_area_cano2 x y)
        (drawr x y)
        (calc_proxim_punt_cano2) ;; Funció per calcular el següent punt
        (sleep 0.075) ;; sleep per que es vegi millor 
        (pintar_trajectoria_cano2))))) ;; tornam al bucle

  

;; Definim quan el punt hauria d'aturar la trajectòria
(defun punt_toca_area (x y)
  (or
    (> x 640) (< x 0) ;; si x se pasa de la pantalla
    (< y (get 'camp1 'd)) ;; si y se pasa del campo 2
    (< y (get 'camp2 'f)) ;; si y se pasa del campo 1
    ;; mur
    (and  (> x (get 'camp1 'a)) ;; esquerra
          (< x (+ (get 'mur 'b) (get 'camp1 'a))) ;; dreta
          (< y (get 'mur 'e))))) ;; adalt

;; Definim quan el punt hauria d'aturar la trajectòria disparada des del canó 2
(defun punt_toca_area_cano2 (x y)
  (or
    (< x 0) (> x 640) ;; si x se pasa de la pantalla
    (< y (get 'camp1 'd)) ;; si y se pasa del campo 2
    (< y (get 'camp2 'f)) ;; si y se pasa del campo 1
    ;; mur
    (and  (< x (+ (get 'mur 'b) (get 'camp1 'a))) ;; esquerra
          (> x (get 'camp1 'a)) ;; dreta
          (< y (get 'mur 'e))))) ;; adalt

;; Comprova si la bala toca el canó 2
(defun punt_toca_cano2 (x y)
  (and  (> x (get 'base_cano2 'pos_x)) ;; la paret esquerra
        (< x (+ (get 'base_cano2 'ample) (get 'base_cano2 'pos_x))) ;; la paret dreta
        (<= y (+ (get 'camp2 'f) (get 'base_cano2 'altura) 5)))) ;; el soterrat (añadir marge)

;; Comprova si la bala toca el canó 1
(defun punt_toca_cano1 (x y)
  (and  (> x (get 'base_cano1 'terc_central)) ;; la paret esquerra
        (< x (+ (get 'base_cano1 'ample) (get 'base_cano1 'terc_central))) ;; la paret dreta
        (<= y (+ (get 'camp1 'd) (get 'base_cano1 'altura) 5)))) ;; el soterrat (añadir marge)




;; Funció que aumenta el dt i calcula les velocitats i posicions
;; en funció de les anteriors
(defun calc_proxim_punt_cano1 ()
        (putprop 'traj (+ (get 'traj 'dt) 0.1) 'dt) ;; aumentam el temps
        (putprop 'traj_cano1 (calcular_vx_1 (get 'traj_cano1 'v_ix) (get 'traj 'dt)) 'v_ix)
        (putprop 'traj_cano1 (calcular_vy_1 (get 'traj_cano1 'v_iy) (get 'traj 'dt)) 'v_iy)
        (putprop 'cano1 (calcular_px_1 (get 'cano1 'x) (get 'traj_cano1 'v_ix) (get 'traj 'dt)) 'x)
        (putprop 'cano1 (calcular_px_2 (get 'cano1 'y)(get 'traj_cano1 'v_iy) (get 'traj 'dt) ) 'y))

(defun calc_proxim_punt_cano2 ()
        (putprop 'traj (+ (get 'traj 'dt) 0.1) 'dt) ;; aumentam el temps
        (putprop 'traj_cano2 (calcular_vx_2 (get 'traj_cano2 'v_ix) (get 'traj 'dt)) 'v_ix)
        (putprop 'traj_cano2 (calcular_vy_2 (get 'traj_cano2 'v_iy) (get 'traj 'dt)) 'v_iy)
        (putprop 'cano2 (calcular_px_1 (get 'cano2 'x) (get 'traj_cano2 'v_ix) (get 'traj 'dt)) 'x)
        (putprop 'cano2 (calcular_px_2 (get 'cano2 'y)(get 'traj_cano2 'v_iy) (get 'traj 'dt) ) 'y))

; Això és un bucle iteratiu. NO EL FEIM SERVIR ENLLOC MÉS
(defun sleep (seconds)
    (do ((endtime (+ (get-internal-real-time)
    (* seconds internal-time-units-per-second))))
        ((> (get-internal-real-time) endtime))))


;; ------ FUNCIONS AUXILIARS PER DEFINIR LES PROPIETATS ------

;; L'amplada del camp1 serà la resultant de restarli 
;; la diferencia aleatòria a la superfície restant i dividir aquesta entre 2.
(defun amplada_camp_1 ()
(round (/ (- (- (get 'superficie 'ample) (get 'mur 'b)) (get 'camps 'dif)) 2)))

;; L'amplada del camp2 serà la resultant de sumarli 
;; la diferència obtinguda, a la posició del camp1.
(defun amplada_camp_2 ()
(+ (amplada_camp_1) (get 'camps 'dif)))

;; Calcula la posició del eix x en funció del punt aleatori del terç central
;; El canó 2 estarà a una distancia: (superficie - (amplada camp2 - punt aleatori cano 2))
(defun pos_x_base_cano2 ()
(- (get 'superficie 'ample) (- (get 'camp2 'c) (get 'base_cano2 'terc_central))))

;; Genera un número aleatori entre min i max, ambdós inclosos.
(defun rnd (min max)
(floor (+ (* (random 1.0) (+ (- max min) 1)) min)))


;; ------ FUNCIONS PER DIBUIXAR FORMES ------

;; Si es vol una línea horizontal h = 0; Si es vol una línea vertical w = 0.
(defun linea (x y w h)
(move x y ) 
(drawrel w 0) (drawrel 0 h))

;; X: distància horizontal al punt 0,0 ; Y: distància vertical al punt 0,0 ; W: amplada ; H: altura
;; Ens movem a les coordenades (x,y) i pintam w, h, -w,-h.
(defun rectangle (x y w h)
(move x y) 
(drawrel w 0) (drawrel 0 h) (drawrel (- w) 0) (drawrel 0 (- h)))

(defun drawr (x y)
(draw (round x) (round y)))

(defun orientacio (x y vi angle)
(move x y)
(drawr (+ x (* vi (cos (radians angle))))(+ y (* vi (sin (radians angle))))))

(defun radians (graus)
(/ (* graus (* 2 pi)) 360))

;; ------ FUNCIONS PER PINTAR ELS ELEMENTS DEL ESCENARI ------

(defun pintar_escenari ()
(rectangle 0 0 (get 'superficie 'ample) (get 'superficie 'alt)))

(defun pintar_camp1 ()
(rectangle 0 0 (get 'camp1 'a) (get 'camp1 'd)))

(defun pintar_camp2 ()
(rectangle (+ (get 'camp1 'a)(get 'mur 'b)) 0 (get 'camp2 'c) (get 'camp2 'f)))

(defun pintar_mur ()
(rectangle (get 'camp1 'a) 0 (get 'mur 'b) (get 'mur 'e)))

(defun pintar_base_cano1 ()
(rectangle (get 'base_cano1 'terc_central) (get 'camp1 'd) (get 'base_cano1 'ample) (get 'base_cano1 'altura)))

(defun pintar_base_cano2 ()
(rectangle (get 'base_cano2 'pos_x) (get 'camp2 'f) (get 'base_cano2 'ample) (get 'base_cano2 'altura)))

(defun pintar_cano1 ()
(orientacio   (+ (get 'base_cano1 'terc_central) (/ (get 'base_cano1 'ample) 2))
              (+ (get 'camp1 'd) (get 'base_cano1 'altura))
              (round(/ (get 'cano1 'v_inicial) 2)) ;; A escala 2:1
              (get 'cano1 'angle)))

(defun pintar_cano2 ()
(orientacio   (+ (get 'base_cano2 'pos_x) (/ (get 'base_cano2 'ample) 2))
              (+ (get 'camp2 'f) (get 'base_cano2 'altura))
              (round(/ (get 'cano2 'v_inicial) 2)) ;; A escala 2:1
              (get 'cano2 'angle)))

(defun pintar_pal ()
(linea  (+ (get 'camp1 'a ) (round ( / (get 'mur 'b) 2))) (+ (get 'mur 'e)) 0 (get 'pal 'altura)))

;; Funció que pinta el contorn de la bandera
;; Es pinta un rectangle partint de la posició pos_i amb una amplada = forza vent i una altura seleccionada
(defun pintar_contorn (pos_i vent)
(rectangle pos_i (+ (get 'mur 'e) (get 'pal 'altura)) vent (get 'bandera 'altura)) 
)

;; Funció recursiva que pinta les linees verticals de la bandera
(defun pintar_bandera (pos_i vent)
(cond
;; Cas base: no queden més línies per pintar
    ((= vent 0))
;; Pas recursiu: encara queden línies per pintar 
    (t 
    ;; pintam linea vertical 
    (linea pos_i (+ (get 'mur 'e) (get 'pal 'altura)) 0 (get 'bandera 'altura)) 
    ;; cridam recursivament a la funció moguent la posicio cap a la dreta
    ;; i reduint 1 nombre la forza del vent (a escala)
    (pintar_bandera (+ pos_i (get 'bandera 'escala)) (- vent (get 'bandera 'escala))))))  

  ;; pos_x representa el punt d'abaix a l'esquerra de la bandera,
  ;; de tal forma que sempre pintarem cap a la dreta per reutilitzar el mètode
  ;; si la forza es neg la pos_x estarà tant a l'esquerra del punt central com forza neg hi hagi
  ;; si la forza es pos la pos_x serà igual a la pos_central.
(defun pintar_bandera_final ()
(cond
;; si la forza es negativa pintarem la bandera cap a la esquerra (emprarem pos_x_esq)
((< (get 'bandera 'forza_vent) 0) 
(pintar_contorn (get 'bandera 'pos_esq_x)(abs (*(get 'bandera 'forza_vent) (get 'bandera 'escala))))
(pintar_bandera (get 'bandera 'pos_esq_x)(abs (*(get 'bandera 'forza_vent) (get 'bandera 'escala))))
)
;; si la forza del vent es + la pinarem a la dreta (emprarem pos_x)
((> (get 'bandera 'forza_vent) 0) 
(pintar_contorn (get 'bandera 'pos_central_x)(abs (*(get 'bandera 'forza_vent) (get 'bandera 'escala))))
(pintar_bandera (get 'bandera 'pos_central_x)(abs (*(get 'bandera 'forza_vent) (get 'bandera 'escala))))
)))


