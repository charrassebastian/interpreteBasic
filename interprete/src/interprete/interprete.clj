(ns interprete.interprete
  (:gen-class))

(declare driver-loop)                     ; NO TOCAR
(declare string-a-tokens)                 ; NO TOCAR
(declare evaluar-linea)                   ; NO TOCAR
(declare buscar-mensaje)                  ; NO TOCAR
(declare seleccionar-destino-de-on)       ; NO TOCAR
(declare leer-data)                       ; NO TOCAR
(declare leer-con-enter)                  ; NO TOCAR
(declare retornar-al-for)                 ; NO TOCAR
(declare continuar-programa)              ; NO TOCAR
(declare ejecutar-programa)               ; NO TOCAR
(declare mostrar-listado)                 ; NO TOCAR
(declare cargar-arch)                     ; NO TOCAR
(declare grabar-arch)                     ; NO TOCAR
(declare calcular-expresion)              ; NO TOCAR
(declare desambiguar-mas-menos)           ; NO TOCAR
(declare desambiguar-mid)                 ; NO TOCAR
(declare shunting-yard)                   ; NO TOCAR
(declare calcular-rpn)                    ; NO TOCAR
(declare imprimir)                        ; NO TOCAR
(declare desambiguar-comas)               ; NO TOCAR

(declare evaluar)                         ; COMPLETAR - hecho
(declare aplicar)                         ; COMPLETAR - hecho

(declare spy)

(declare palabra-reservada?)              ; IMPLEMENTAR - hecho
(declare operador?)                       ; IMPLEMENTAR - hecho
(declare anular-invalidos)                ; IMPLEMENTAR - hecho
(declare cargar-linea)                    ; IMPLEMENTAR - hecho
(declare expandir-nexts)                  ; IMPLEMENTAR - hecho
(declare dar-error)                       ; IMPLEMENTAR - hecho
(declare variable-float?)                 ; IMPLEMENTAR - hecho
(declare variable-integer?)               ; IMPLEMENTAR - hecho
(declare variable-string?)                ; IMPLEMENTAR - hecho
(declare contar-sentencias)               ; IMPLEMENTAR - hecho
(declare buscar-lineas-restantes)         ; IMPLEMENTAR - hecho
(declare continuar-linea)                 ; IMPLEMENTAR - hecho
(declare extraer-data)                    ; IMPLEMENTAR - hecho
(declare ejecutar-asignacion)             ; IMPLEMENTAR - hecho
(declare preprocesar-expresion)           ; IMPLEMENTAR - hecho
(declare desambiguar)                     ; IMPLEMENTAR - hecho
(declare precedencia)                     ; IMPLEMENTAR - hecho
(declare aridad)                          ; IMPLEMENTAR - hecho
(declare eliminar-cero-decimal)           ; IMPLEMENTAR - hecho
(declare eliminar-cero-entero)            ; IMPLEMENTAR - hecho

;; funciones auxiliares
(declare nombre-variable-valido?)
(declare generar-msg-error)
(declare contar-sentencias-aux)
(declare expandir-solo-nexts-primera-linea)
(declare extraer-data-sentencia)
(declare extraer-data-linea)
(declare flatten-primer-nivel)
(declare insertar-separadores-sentencias)

(defn -main
  [& args]
  (driver-loop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; driver-loop: el REPL del interprete de Commodore 64 BASIC V2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn driver-loop
  ([]
   (println)
   (println "Interprete de BASIC en Clojure")
   (println "Trabajo Practico de Programacion III - 3.602 - 2023")
   (println)
   (println "Inspirado en:  ******************************************")
   (println "               *                                        *")
   (println "               *    **** COMMODORE 64 BASIC V2 ****     *")
   (println "               *                                        *")
   (println "               * 64K RAM SYSTEM  38911 BASIC BYTES FREE *")
   (println "               *                                        *")
   (println "               ******************************************")
   (flush)
   (driver-loop ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]))  ; [(prog-mem)  [prog-ptrs]  [gosub-return-stack]  [for-next-stack]  [data-mem]  data-ptr  {var-mem}]
  ([amb]
   (prn) (println "READY.") (flush)
   (try (let [linea (string-a-tokens (read-line)), cabeza (first linea)]
          (cond (= cabeza '(EXIT)) 'GOODBYE
                (= cabeza '(ENV)) (do (prn amb) (flush) (driver-loop amb))
                (integer? cabeza) (if (and (>= cabeza 0) (<= cabeza 63999))
                                    (driver-loop (cargar-linea linea amb))
                                    (do (dar-error 16 (amb 1)) (driver-loop amb))) ; Syntax error
                (empty? linea) (driver-loop amb)
                :else (driver-loop (second (evaluar-linea linea (assoc amb 1 [:ejecucion-inmediata (count (expandir-nexts linea))]))))))
        (catch Exception e (dar-error (str "?ERROR " (clojure.string/trim (clojure.string/upper-case (get (Throwable->map e) :cause)))) (amb 1)) (driver-loop amb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; string-a-tokens: analisis lexico y traduccion del codigo a la
; representacion intermedia (listas de listas de simbolos) que
; sera ejecutada (o vuelta atras cuando deba ser mostrada)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn string-a-tokens [s]
  (let [nueva (str s ":"),
        mayu (clojure.string/upper-case nueva),
        sin-cad (clojure.string/replace mayu #"\"(.*?)\"" #(clojure.string/join (take (+ (count (% 1)) 2) (repeat "@")))),
        ini-rem (clojure.string/index-of sin-cad "REM"),
        pre-rem (subs mayu 0 (if (nil? ini-rem) (count mayu) ini-rem))
        pos-rem (subs mayu (if (nil? ini-rem) (- (count mayu) 1) (+ ini-rem 3)) (- (count mayu) 1))
        sin-rem (->> pre-rem
                     (re-seq #"EXIT|ENV|DATA[^\:]*?\:|REM|NEW|CLEAR|LIST|RUN|LOAD|SAVE|LET|AND|OR|NOT|ABS|SGN|INT|SQR|SIN|COS|TAN|ATN|EXP|LOG|LEN|LEFT\$|MID\$|RIGHT\$|STR\$|VAL|CHR\$|ASC|GOTO|ON|IF|THEN|FOR|TO|STEP|NEXT|GOSUB|RETURN|END|INPUT|READ|RESTORE|PRINT|\<\=|\=\<|\>\=|\=\>|\<\>|\>\<|\<|\>|\=|\(|\)|\?|\;|\:|\,|\+|\-|\*|\/|\^|\"[^\"]*\"|\d+\.\d+E[+-]?\d+|\d+\.E[+-]?\d+|\.\d+E[+-]?\d+|\d+E[+-]?\d+|\d+\.\d+|\d+\.|\.\d+|\.|\d+|[A-Z][A-Z0-9]*[\%\$]?|[A-Z]|\!|\"|\#|\$|\%|\&|\'|\@|\[|\\|\]|\_|\{|\||\}|\~")
                     (map #(if (and (> (count %) 4) (= "DATA" (subs % 0 4))) (clojure.string/split % #":") [%]))
                     (map first)
                     (remove nil?)
                     (replace '{"?" "PRINT"})
                     (map #(if (and (> (count %) 1) (clojure.string/starts-with? % ".")) (str 0 %) %))
                     (map #(if (and (>= (count %) 4) (= "DATA" (subs % 0 4))) (let [provisorio (interpose "," (clojure.string/split (clojure.string/triml (subs % 4)) #",[ ]*"))] (list "DATA" (if (= ((frequencies %) \,) ((frequencies provisorio) ",")) provisorio (list provisorio ",")) ":")) %))
                     (flatten)
                     (map #(let [aux (try (clojure.edn/read-string %) (catch Exception e (symbol %)))] (if (or (number? aux) (string? aux)) aux (symbol %))))
                     (#(let [aux (first %)] (if (and (integer? aux) (not (neg? aux))) (concat (list aux) (list (symbol ":")) (rest %)) %)))
                     (partition-by #(= % (symbol ":")))
                     (remove #(.contains % (symbol ":")))
                     (#(if (and (= (count (first %)) 1) (number? (ffirst %))) (concat (first %) (rest %)) %)))]
    (if (empty? pos-rem)
      sin-rem
      (concat sin-rem (list (list 'REM (symbol (clojure.string/trim pos-rem))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evaluar-linea: recibe una lista de sentencias y las evalua
; mientras sea posible hacerlo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn evaluar-linea
  ([sentencias amb]
   (let [sentencias-con-nexts-expandidos (expandir-nexts sentencias)]
     (evaluar-linea sentencias-con-nexts-expandidos sentencias-con-nexts-expandidos amb)))
  ([linea sentencias amb]
   (if (empty? sentencias)
     [:sin-errores amb]
     (let [sentencia (anular-invalidos (first sentencias)), par-resul (evaluar sentencia amb)]
       (if (or (nil? (first par-resul)) (contains? #{:omitir-restante, :error-parcial, :for-inconcluso} (first par-resul)))
         (if (and (= (first (amb 1)) :ejecucion-inmediata) (= (first par-resul) :for-inconcluso))
           (recur linea (take-last (second (second (second par-resul))) linea) (second par-resul))
           par-resul)
         (recur linea (next sentencias) (assoc (par-resul 1) 1 [(first (amb 1)) (count (next sentencias))])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; buscar-mensaje: retorna el mensaje correspondiente a un error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn buscar-mensaje [cod]
  (case cod
    0 "?NEXT WITHOUT FOR  ERROR"
    6 "FILE NOT FOUND"
    15 "NOT DIRECT COMMAND"
    16 "?SYNTAX  ERROR"
    22 "?RETURN WITHOUT GOSUB  ERROR"
    42 "?OUT OF DATA  ERROR"
    53 "?ILLEGAL QUANTITY  ERROR"
    69 "?OVERFLOW  ERROR"
    90 "?UNDEF'D STATEMENT  ERROR"
    100 "?ILLEGAL DIRECT  ERROR"
    133 "?DIVISION BY ZERO  ERROR"
    163 "?TYPE MISMATCH  ERROR"
    176 "?STRING TOO LONG  ERROR"
    200 "?LOAD WITHIN PROGRAM  ERROR"
    201 "?SAVE WITHIN PROGRAM  ERROR"
    cod))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; seleccionar-destino-de-on: recibe una lista de numeros
; separados por comas, un indice y el ambiente, y retorna el
; numero a que hace referencia el indice (se cuenta desde 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn seleccionar-destino-de-on
  ([destinos indice amb]
   (cond
     (or (neg? indice) (> indice 255)) (do (dar-error 53 (amb 1)) nil)  ; Illegal quantity error
     (zero? indice) :omitir-restante
     :else (seleccionar-destino-de-on (if (= (last destinos) (symbol ",")) (concat destinos [0]) destinos) indice amb 1)))
  ([destinos indice amb contador]
   (cond
     (nil? destinos) :omitir-restante
     (= contador indice) (if (= (first destinos) (symbol ",")) 0 (first destinos))
     (= (first destinos) (symbol ",")) (recur (next destinos) indice amb (inc contador))
     (or (= (count destinos) 1)
         (and (> (count destinos) 1) (= (second destinos) (symbol ",")))) (recur (nnext destinos) indice amb (inc contador))
     :else (do (dar-error 16 (amb 1)) nil)))  ; Syntax error
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; leer-data: recibe una lista de variables separadas por comas
; y un ambiente, y retorna una dupla (un vector) con un
; resultado (usado luego por evaluar-linea) y un ambiente
; actualizado incluyendo las variables cargadas con los valores
; definidos en la(s) sentencia(s) DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn leer-data
  ([param-de-read amb]
   (cond
     (= (first (amb 1)) :ejecucion-inmediata) (do (dar-error 15 (amb 1)) [nil amb])  ; Not direct command
     (empty? param-de-read) (do (dar-error 16 (amb 1)) [nil amb])  ; Syntax error
     :else (leer-data param-de-read (drop (amb 5) (amb 4)) amb)))
  ([variables entradas amb]
   (cond
     (empty? variables) [:sin-errores amb]
     (empty? entradas) (do (dar-error 42 (amb 1)) [:error-parcial amb])  ; Out of data error
     :else (let [res (ejecutar-asignacion (list (first variables) '= (if (variable-string? (first variables)) (str (first entradas)) (if (= (first entradas) "") 0 (first entradas)))) amb)]
             (if (nil? res)
               [nil amb]
               (if (or (= (count (next variables)) 1)
                       (and (> (count (next variables)) 1) (not= (fnext variables) (symbol ","))))
                 (do (dar-error 16 (amb 1)) [:error-parcial res])  ; Syntax error
                 (recur (nnext variables) (next entradas) (assoc res 5 (inc (res 5))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; leer-con-enter: recibe una lista con una cadena opcional
; seguida de variables separadas por comas y un ambiente, y
; retorna una dupla (un vector) con un resultado (usado luego
; por evaluar-linea) y un ambiente actualizado incluyendo las
; variables cargadas con los valores leidos del teclado
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn leer-con-enter
  ([param-de-input amb]
   (leer-con-enter param-de-input param-de-input amb))
  ([param-orig param-actualizados amb]
   (let [prim-arg (first param-actualizados), es-cadena (string? prim-arg)]
     (if (and es-cadena (not= (second param-actualizados) (symbol ";")))
       (do (dar-error 16 (amb 1)) [nil amb])  ; Syntax error
       (do (if es-cadena
             (print (str prim-arg "? "))
             (print "? "))
           (flush)
           (if (= (first (amb 1)) :ejecucion-inmediata)
             (do (dar-error 100 (amb 1)) [nil amb])  ; Illegal direct error
             (let [variables (if es-cadena (nnext param-actualizados) param-actualizados),
                   valores (butlast (map clojure.string/trim (.split (apply str (.concat (read-line) ",.")) ","))),
                   entradas (map #(let [entr (try (clojure.edn/read-string %) (catch Exception e (str %)))] (if (number? entr) entr (clojure.string/upper-case (str %)))) valores)]
               (if (empty? variables)
                 (do (dar-error 16 (amb 1)) [nil amb])  ; Syntax error
                 (leer-con-enter variables entradas param-orig param-actualizados amb amb))))))))
  ([variables entradas param-orig param-actualizados amb-orig amb-actualizado]
   (cond
     (and (empty? variables) (empty? entradas)) [:sin-errores amb-actualizado]
     (and (empty? variables) (not (empty? entradas))) (do (println "?EXTRA IGNORED") (flush) [:sin-errores amb-actualizado])
     (and (not (empty? variables)) (empty? entradas)) (leer-con-enter param-orig (concat (list "?? " (symbol ";")) variables) amb-actualizado)
     (and (not (variable-string? (first variables))) (string? (first entradas))) (do (println "?REDO FROM START") (flush) (leer-con-enter param-orig param-orig amb-orig))
     :else (let [res (ejecutar-asignacion (list (first variables) '= (if (variable-string? (first variables)) (str (first entradas)) (first entradas))) amb-actualizado)]
             (if (nil? res)
               [nil amb-actualizado]
               (if (or (= (count (next variables)) 1)
                       (and (> (count (next variables)) 1) (not= (fnext variables) (symbol ","))))
                 (do (dar-error 16 (amb-actualizado 1)) [:error-parcial res])  ; Syntax error
                 (recur (nnext variables) (next entradas) param-orig param-actualizados amb-orig res)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; retornar-al-for: implementa la sentencia NEXT, retornando una
; dupla (un vector) con un resultado (usado luego por
; evaluar-linea) y un ambiente actualizado con el nuevo valor
; de la variable de control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defn retornar-al-for [amb var-next]
  (if (empty? (amb 3))
    (do (dar-error 0 (amb 1)) [nil amb])  ; Next without for error
    (let [datos-for (peek (amb 3)),
          var-for (nth datos-for 0),
          valor-final (nth datos-for 1),
          valor-step (nth datos-for 2),
          origen (nth datos-for 3)]
      (if (and (some? var-next) (not= var-next var-for))
        (retornar-al-for (assoc amb 3 (pop (amb 3))) var-next)
        (let [var-actualizada (+ (calcular-expresion (list var-for) amb) valor-step),
              res (ejecutar-asignacion (list var-for '= var-actualizada) amb)]
          (if (or (and (neg? valor-step) (>= var-actualizada valor-final))
                  (and (pos? valor-step) (<= var-actualizada valor-final)))
            [:for-inconcluso (assoc res 1 [(origen 0) (dec (origen 1))])]
            [:sin-errores (assoc res 3 (pop (amb 3)))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; continuar-programa: recibe un ambiente que fue modificado por
; GOTO o GOSUB y continua la ejecucion del programa a partir de
; ese ambiente
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn continuar-programa [amb]
  (ejecutar-programa amb (buscar-lineas-restantes amb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ejecutar-programa: recibe un ambiente e inicia la ejecucion
; del programa a partir de ese ambiente 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ejecutar-programa
  ([amb]
   (let [ini [(amb 0) (amb 1) [] [] (vec (extraer-data (amb 0))) 0 {}]]  ; [(prog-mem)  [prog-ptrs]  [gosub-return-stack]  [for-next-stack]  [data-mem]  data-ptr  {var-mem}]
     (ejecutar-programa ini (buscar-lineas-restantes ini))))
  ([amb prg]
   (if (or (nil? prg) (= (first (amb 1)) :ejecucion-inmediata))
     [:sin-errores amb]
     (let [antes (assoc amb 1 [(ffirst prg) (second (amb 1))]), res (evaluar-linea (nfirst prg) antes), nuevo-amb (second res)]
       (cond (nil? (first res)) [nil amb]   ; hubo error total 
             (= (first res) :error-parcial) [nil (second res)]   ; hubo error parcial
             :else (let [proximo (if (and (= (first (antes 1)) (first (nuevo-amb 1))) (not= (first res) :for-inconcluso))
                                   (next prg)   ; no hubo quiebre de secuencia
                                   (buscar-lineas-restantes nuevo-amb)),
                         nueva-posic (if (nil? proximo) (nuevo-amb 1) [(ffirst proximo) (count (expandir-nexts (nfirst proximo)))])]
                     (recur (assoc nuevo-amb 1 nueva-posic) proximo)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; mostrar-listado: recibe la representacion intermedia de un
; programa y lo lista usando la representacion normal
; (usualmente mas legible que la ingresada originalmente)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mostrar-listado
  ([lineas]
   (if (empty? lineas)
     nil
     (mostrar-listado (next lineas) (first lineas))))
  ([lineas sentencias]
   (if (empty? sentencias)
     (do (prn) (mostrar-listado lineas))
     (mostrar-listado lineas (next sentencias) (first sentencias))))
  ([lineas sentencias elementos]
   (if (and (not (seq? elementos)) (integer? elementos))
     (do (pr elementos) (print "  ") (mostrar-listado lineas sentencias))
     (if (empty? elementos)
       (do (if (not (empty? sentencias)) (print ": "))
           (mostrar-listado lineas sentencias))
       (do (pr (first elementos))
           (if (not (or (contains? #{(symbol "(") (symbol ",") (symbol ";")} (first elementos))
                        (contains? #{(symbol ")") (symbol ",") (symbol ";")} (fnext elementos))
                        (nil? (fnext elementos)))) (print " "))
           (recur lineas sentencias (next elementos)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cargar-arch: recibe un nombre de archivo y retorna la
; representacion intermedia del codigo contenido en el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cargar-arch [nom nro-linea]
  (if (.exists (clojure.java.io/file nom))
    (remove empty? (with-open [rdr (clojure.java.io/reader nom)] (doall (map string-a-tokens (line-seq rdr)))))
    (dar-error 6 nro-linea))  ; File not found
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; grabar-arch: recibe un nombre de archivo, graba en el
; el listado del programa usando la representacion normal y
; retorna el ambiente
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn grabar-arch [nom amb]
  (let [arch (clojure.java.io/writer nom)]
    (do (binding [*out* arch] (mostrar-listado (amb 0)))
        (.close arch)
        amb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; calcular-expresion: recibe una expresion y un ambiente, y
; retorna el valor de la expresion, por ejemplo:
; user=> (calcular-expresion '(X + 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
; 7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcular-expresion [expr amb]
  (calcular-rpn 
   (shunting-yard 
    (desambiguar 
     (preprocesar-expresion expr amb))) (amb 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; desambiguar-mas-menos: recibe una expresion y la retorna sin
; los + unarios y con los - unarios reemplazados por -u  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn desambiguar-mas-menos
  ([expr] (desambiguar-mas-menos expr nil []))
  ([expr ant res]
   (if (nil? expr)
     (remove nil? res)
     (let [act (first expr), nuevo (if (or (nil? ant) (and (symbol? ant) (operador? ant)) (= (str ant) "(") (= (str ant) ","))
                                     (case act
                                       + nil
                                       - '-u
                                       act)
                                     act)]
       (recur (next expr) act (conj res nuevo))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; desambiguar-mid: recibe una expresion y la retorna con los
; MID$ ternarios reemplazados por MID3$ 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn desambiguar-mid
  ([expr]
   (cond
     (contains? (set expr) 'MID$) (desambiguar-mid expr 0 (count expr) 0 0 0 true)
     (contains? (set expr) 'MID2$) (apply list (replace '{MID2$ MID$} expr))
     :else (apply list expr)))
  ([expr act fin pos cont-paren cont-comas buscando]
   (if (= act fin)
     (desambiguar-mid expr)
     (let [nuevo (nth expr act)]
       (cond
         (and (= nuevo 'MID$) buscando) (recur expr (inc act) fin act cont-paren cont-comas false)
         (and (= nuevo (symbol "(")) (not buscando)) (recur expr (inc act) fin pos (inc cont-paren) cont-comas buscando)
         (and (= nuevo (symbol ")")) (not buscando))
         (if (= cont-paren 1)
           (if (= cont-comas 2)
             (recur (assoc (vec expr) pos 'MID3$) (inc act) fin 0 0 0 true)
             (recur (assoc (vec expr) pos 'MID2$) (inc act) fin 0 0 0 true))
           (recur expr (inc act) fin pos (dec cont-paren) cont-comas buscando))
         (and (= nuevo (symbol ",")) (= cont-paren 1)) (recur expr (inc act) fin pos cont-paren (inc cont-comas) buscando)
         :else (recur expr (inc act) fin pos cont-paren cont-comas buscando))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; shunting-yard: implementa el algoritmo del Patio de Maniobras
; de Dijkstra que convierte una expresion a RPN (Reverse Polish
; Notation), por ejemplo:
; user=> (shunting-yard '(1 + 2))
; (1 2 +)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn shunting-yard [tokens]
  (remove #(= % (symbol ","))
          (flatten
           (reduce
            (fn [[rpn pila] token]
              (let [op-mas? #(and (some? (precedencia %)) (>= (precedencia %) (precedencia token)))
                    no-abre-paren? #(not= (str %) "(")]
                (cond
                  (= (str token) "(") [rpn (cons token pila)]
                  (= (str token) ")") [(vec (concat rpn (take-while no-abre-paren? pila))) (rest (drop-while no-abre-paren? pila))]
                  (some? (precedencia token)) [(vec (concat rpn (take-while op-mas? pila))) (cons token (drop-while op-mas? pila))]
                  :else [(conj rpn token) pila])))
            [[] ()]
            tokens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; calcular-rpn: Recibe una expresion en RPN y un numero de linea
; y retorna el valor de la expresion o un mensaje de error en la
; linea indicada
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn calcular-rpn [tokens nro-linea]
  (try
    (let [resu-redu
          (reduce
           (fn [pila token]
             (let [ari (aridad token),
                   resu (eliminar-cero-decimal
                          (case ari
                           1 (aplicar token (first pila) nro-linea)
                           2 (aplicar token (second pila) (first pila) nro-linea)
                           3 (aplicar token (nth pila 2) (nth pila 1) (nth pila 0) nro-linea)
                           token))]
               (if (nil? resu)
                 (reduced resu)
                 (cons resu (drop ari pila)))))
           [] tokens)]
      (if  (> (count resu-redu) 1)
        (dar-error 16 nro-linea)  ; Syntax error
        (first resu-redu)))
    (catch NumberFormatException e 0)
    (catch ClassCastException e (dar-error 163 nro-linea)) ; Type mismatch error
    (catch UnsupportedOperationException e (dar-error 163 nro-linea)) ; Type mismatch error
    (catch IllegalArgumentException e (dar-error 69 nro-linea))  ; Overflow error 
    (catch Exception e (dar-error 16 nro-linea)))  ; Syntax error
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; imprimir: recibe una lista de expresiones (separadas o no
; mediante puntos y comas o comas) y un ambiente, y las muestra
; interpretando los separadores como tabulaciones (las comas) o
; concatenaciones (los puntos y comas). Salvo cuando la lista
; termina en punto y coma, imprime un salto de linea al terminar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn imprimir
  ([v]
   (let [expresiones (v 0), amb (v 1)]
     (cond
       (empty? expresiones) (do (prn) (flush) :sin-errores)
       (and (empty? (next expresiones)) (= (first expresiones) (list (symbol ";")))) (do (pr) (flush) :sin-errores)
       (and (empty? (next expresiones)) (= (first expresiones) (list (symbol ",t")))) (do (printf "\t\t") (flush) :sin-errores)
       (= (first expresiones) (list (symbol ";"))) (do (pr) (flush) (recur [(next expresiones) amb]))
       (= (first expresiones) (list (symbol ",t"))) (do (printf "\t\t") (flush) (recur [(next expresiones) amb]))
       :else (let [resu (eliminar-cero-entero (calcular-expresion (first expresiones) amb))]
               (if (nil? resu)
                 resu
                 (do (print resu) (flush) (recur [(next expresiones) amb])))))))
  ([lista-expr amb]
   (let [nueva (cons (conj [] (first lista-expr)) (rest lista-expr)),
         variable? #(or (variable-integer? %) (variable-float? %) (variable-string? %)),
         funcion? #(and (> (aridad %) 0) (not (operador? %))),
         interc (reduce #(if (and (or (number? (last %1)) 
                                      (string? (last %1)) 
                                      (variable? (last %1))
                                      (= (symbol ")") (last %1)))
                                  (or (number? %2) 
                                      (string? %2) 
                                      (variable? %2) 
                                      (funcion? %2) 
                                      (= (symbol "(") %2)))
                           (conj (conj %1 (symbol ";")) %2) 
                           (conj %1 %2)) nueva),
         ex (partition-by #(= % (symbol ",t")) (desambiguar-comas interc)),
         expresiones (apply concat (map #(partition-by (fn [x] (= x (symbol ";"))) %) ex))]
     (imprimir [expresiones amb]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; desambiguar-comas: recibe una expresion en forma de lista y
; la devuelve con las comas que esten afuera de los pares de
; parentesis remplazadas por el simbolo ,t (las demas, que se
; usan para separar argumentos, se mantienen intactas)  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn desambiguar-comas
  ([lista-expr]
   (desambiguar-comas lista-expr 0 []))
  ([lista-expr cont-paren res]
   (if (nil? lista-expr)
     res
     (let [act (first lista-expr),
           paren (cond
                   (= act (symbol "(")) (inc cont-paren)
                   (= act (symbol ")")) (dec cont-paren)
                   :else cont-paren),
           nuevo (if (and (= act (symbol ",")) (zero? paren)) (symbol ",t") act)]
       (recur (next lista-expr) paren (conj res nuevo))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A PARTIR DE ESTE PUNTO HAY QUE COMPLETAR LAS FUNCIONES DADAS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; evaluar: ejecuta una sentencia y retorna una dupla (un vector)
; con un resultado (usado luego por evaluar-linea) y un ambiente
; actualizado
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn evaluar [sentencia amb]
  (if (or (contains? (set sentencia) nil) (and (palabra-reservada? (first sentencia)) (= (second sentencia) '=)))
    (do (dar-error 16 (amb 1)) [nil amb])  ; Syntax error  
    (case (first sentencia)
      PRINT (let [args (next sentencia), resu (if (nil? args)
                                                (do (println) nil)
                                                (imprimir args amb))]
              (if (and (nil? resu) (some? args))
                [nil amb]
                [:sin-errores amb]))
      LOAD (if (= (first (amb 1)) :ejecucion-inmediata)
             (let [nuevo-amb (cargar-arch (apply str (next sentencia)) (amb 1))]
               (if (nil? nuevo-amb)
                 [nil amb]
                 [:sin-errores [nuevo-amb [:ejecucion-inmediata 0] [] [] [] 0 {}]]))  ; [(prog-mem)  [prog-ptrs]  [gosub-return-stack]  [for-next-stack]  [data-mem]  data-ptr  {var-mem}]
             (do (dar-error 200 (amb 1)) [nil amb]))  ; Load within program error
      SAVE (if (= (first (amb 1)) :ejecucion-inmediata)
             (let [resu (grabar-arch (apply str (next sentencia)) amb)]
               (if (nil? resu)
                 [nil amb]
                 [:sin-errores amb]))
             (do (dar-error 201 (amb 1)) [nil amb]))  ; Save within program error
      REM [:omitir-restante amb]
      NEW [:sin-errores ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]]  ; [(prog-mem)  [prog-ptrs]  [gosub-return-stack]  [for-next-stack]  [data-mem]  data-ptr  {var-mem}]
      RUN (cond
            (empty? (amb 0)) [:sin-errores amb]  ; no hay programa
            (= (count sentencia) 1) (ejecutar-programa (assoc amb 1 [(ffirst (amb 0)) (count (expandir-nexts (nfirst (amb 0))))]))  ; no hay argumentos   
            (= (count (next sentencia)) 1) (ejecutar-programa (assoc amb 1 [(fnext sentencia) (contar-sentencias (fnext sentencia) amb)]))  ; hay solo un argumento
            :else (do (dar-error 16 (amb 1)) [nil amb]))  ; Syntax error
      GOTO (let [num-linea (if (some? (second sentencia)) (second sentencia) 0)]
             (if (not (contains? (into (hash-set) (map first (amb 0))) num-linea))
               (do (dar-error 90 (amb 1)) [nil amb])  ; Undef'd statement error
               (let [nuevo-amb (assoc amb 1 [num-linea (contar-sentencias num-linea amb)])]
                 (if (= (first (amb 1)) :ejecucion-inmediata)
                   (continuar-programa nuevo-amb)
                   [:omitir-restante nuevo-amb]))))
      IF (let [separados (split-with #(not (contains? #{"THEN" "GOTO"} (str %))) (next sentencia)),
               condicion-de-if (first separados),
               resto-if (second separados),
               sentencia-de-if (cond
                                 (= (first resto-if) 'GOTO) resto-if
                                 (= (first resto-if) 'THEN) (if (number? (second resto-if))
                                                              (cons 'GOTO (next resto-if))
                                                              (next resto-if))
                                 :else (do (dar-error 16 (amb 1)) nil)),  ; Syntax error
               resu (calcular-expresion condicion-de-if amb)]
           (if (zero? resu)
             [:omitir-restante amb]
             (recur sentencia-de-if amb)))
      INPUT (leer-con-enter (next sentencia) amb)
      ON (let [separados (split-with #(not (contains? #{"GOTO" "GOSUB"} (str %))) (next sentencia)),
               indice-de-on (calcular-expresion (first separados) amb),
               sentencia-de-on (first (second separados)),
               destino-de-on (seleccionar-destino-de-on (next (second separados)) indice-de-on amb)]
           (cond
             (nil? destino-de-on) [nil amb]
             (= destino-de-on :omitir-restante) [:sin-errores amb]
             :else (recur (list sentencia-de-on destino-de-on) amb)))
      GOSUB (let [num-linea (if (some? (second sentencia)) (second sentencia) 0)]
              (if (not (contains? (into (hash-set) (map first (amb 0))) num-linea))
                (do (dar-error 90 (amb 1)) [nil amb])  ; Undef'd statement error
                (let [pos-actual (amb 1),
                      nuevo-amb (assoc (assoc amb 1 [num-linea (contar-sentencias num-linea amb)]) 2 (conj (amb 2) pos-actual))]
                  (if (= (first (amb 1)) :ejecucion-inmediata)
                    (continuar-programa nuevo-amb)
                    [:omitir-restante nuevo-amb]))))
      RETURN (continuar-linea amb)
      FOR (let [separados (partition-by #(contains? #{"TO" "STEP"} (str %)) (next sentencia))]
            (if (not (or (and (= (count separados) 3) (variable-float? (ffirst separados)) (= (nth separados 1) '(TO)))
                         (and (= (count separados) 5) (variable-float? (ffirst separados)) (= (nth separados 1) '(TO)) (= (nth separados 3) '(STEP)))))
              (do (dar-error 16 (amb 1)) [nil amb])  ; Syntax error
              (let [valor-final (calcular-expresion (nth separados 2) amb),
                    valor-step (if (= (count separados) 5) (calcular-expresion (nth separados 4) amb) 1)]
                (if (or (nil? valor-final) (nil? valor-step))
                  [nil amb]
                  (recur (first separados) (assoc amb 3 (conj (amb 3) [(ffirst separados) valor-final valor-step (amb 1)])))))))
      NEXT (if (<= (count (next sentencia)) 1)
             (retornar-al-for amb (fnext sentencia))
             (do (dar-error 16 (amb 1)) [nil amb]))  ; Syntax error
      ; completado
      DATA [:omitir-restante amb]
      END (vector :omitir-restante 
                  (let [amb-base ['() [:ejecucion-inmediata 0] [] [] [] 0 '{}]
                        amb-nuevo (assoc amb-base 0 (get amb 0))]
                    amb-nuevo)) ; [(prog-mem)  [prog-ptrs]  [gosub-return-stack]  [for-next-stack]  [data-mem]  data-ptr  {var-mem}]
      ; READ: actualiza la variable en el ambiente, aumenta el puntero
      ; y si no hay mas datos, muestra el ?OUT OF DATA ERROR
      READ (let [data-ptr-viejo (get amb 5)
                 amb-con-ptr-nuevo (assoc amb 5 (inc data-ptr-viejo))
                 data-mem (get amb 4)
                 var-mem-viejo (get amb 6)
                 dato-leido (get data-mem data-ptr-viejo)
                 var-mem-nuevo (assoc var-mem-viejo (fnext sentencia) dato-leido)
                 amb-nuevo (assoc amb-con-ptr-nuevo 6 var-mem-nuevo)]
             (if (nil? dato-leido)
               [:error-parcial amb]
               [:sin-errores amb-nuevo]))
      ; RESTORE: reinicia los datos embebidos, devolviendo un
      ; vector con dos elementos, :sin-errores y el ambiente
      ; con el data-ptr = 0
      RESTORE [:sin-errores (assoc amb 5 0)]
      ; CLEAR: borra todas las variables, devolviendo un
      ; vector con dos elementos, :sin-errores y el ambiente
      ; con var-mem vacio
      CLEAR [:sin-errores (assoc amb 6 {})]
      ; LIST: muestra el listado de sentencias del programa, 
      ; devuelve un vector con dos elementos, :sin-errores y el
      ; ambiente

      ; LIST (do (prn (get amb 0)) (flush) [:sin-errores amb])
      LIST (do (run! println
                     (map (partial apply str)
                          (map butlast
                               (map rest
                                    (map str
                                         (map flatten-primer-nivel (map insertar-separadores-sentencias (get amb 0))))))))
               (flush)
               [:sin-errores amb])

      ; LET/=: hace la asignacion de un valor a una variable,
      ; devuelve un vector con dos elementos, :sin-errores y el
      ; ambiente actualizado
      LET (let [valor-expresion (calcular-expresion (drop 3 sentencia) amb)
                var-mem-viejo (get amb 6)
                var-mem-nuevo (assoc var-mem-viejo (fnext sentencia) valor-expresion)
                amb-nuevo (assoc amb 6 var-mem-nuevo)]
            (if (or (not= '= (nth sentencia 2)) (and (not (number? valor-expresion)) (not (string? valor-expresion))))
              [:error-parcial amb]
              [:sin-errores amb-nuevo]))
      ; hasta aqui
      (if (= (second sentencia) '=)
        (let [resu (ejecutar-asignacion sentencia amb)]
          (if (nil? resu)
            [nil amb]
            [:sin-errores resu]))
        (do (dar-error 16 (amb 1)) [nil amb]))))  ; Syntaxerror
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; aplicar: aplica un operador a sus operandos y retorna el valor
; resultante (si ocurre un error, muestra un mensaje y retorna
; nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aplicar
  ([operador operando nro-linea]
   (if (nil? operando)
     (dar-error 16 nro-linea)  ; Syntax error
     (case operador
       -u (- operando)
       LEN (count operando)
       STR$ (if (not (number? operando)) (dar-error 163 nro-linea) (eliminar-cero-entero operando)) ; Type mismatch error
       CHR$ (if (or (< operando 0) (> operando 255)) (dar-error 53 nro-linea) (str (char operando))) ; Illegal quantity error
       ; desde aqui fui completando
       INT (if (not (number? operando)) (dar-error 163 nro-linea) (int operando))
       SIN (if (not (number? operando)) (dar-error 163 nro-linea) (. Math sin operando))
       EXP (if (not (number? operando)) (dar-error 163 nro-linea) (. Math exp operando))
       LOG (if (not (number? operando)) (dar-error 163 nro-linea) (. Math log operando))
       ASC (if (not (string? operando)) (dar-error 163 nro-linea) (int (get operando 0)))
       ATN (if (not (number? operando)) (dar-error 163 nro-linea) (. Math atan operando)))))
  ([operador operando1 operando2 nro-linea]
   (if (or (nil? operando1) (nil? operando2))
     (dar-error 16 nro-linea)  ; Syntax error
     (case operador
       = (if (and (string? operando1) (string? operando2))
           (if (= operando1 operando2) -1 0)
           (if (= (+ 0 operando1) (+ 0 operando2)) -1 0))
       + (if (and (string? operando1) (string? operando2))
           (str operando1 operando2)
           (+ operando1 operando2))
       - (- operando1 operando2)
       / (if (= operando2 0) (dar-error 133 nro-linea) (/ operando1 operando2))  ; Division by zero error
       AND (let [op1 (+ 0 operando1), op2 (+ 0 operando2)] (if (and (not= op1 0) (not= op2 0)) -1 0))
       MID$ (if (< operando2 1)
              (dar-error 53 nro-linea)  ; Illegal quantity error
              (let [ini (dec operando2)] (if (>= ini (count operando1)) "" (subs operando1 ini))))
       ; desde aqui fui completando
       * (* operando1 operando2) 
       <> (if (and (string? operando1) (string? operando2))
           (if (not= operando1 operando2) -1 0)
           (if (not= (+ 0 operando1) (+ 0 operando2)) -1 0))
       < (if (and (number? operando1) (number? operando2))
           (if (< operando1 operando2) -1 0)
           (dar-error 163 nro-linea))
       <= (if (and (number? operando1) (number? operando2))
           (if (<= operando1 operando2) -1 0)
           (dar-error 163 nro-linea))
       > (if (and (number? operando1) (number? operando2))
           (if (> operando1 operando2) -1 0)
           (dar-error 163 nro-linea))
       >= (if (and (number? operando1) (number? operando2))
           (if (>= operando1 operando2) -1 0)
           (dar-error 163 nro-linea))
       OR (let [op1 (+ 0 operando1), op2 (+ 0 operando2)] (if (or (not= op1 0) (not= op2 0)) -1 0))
       )))
  ([operador operando1 operando2 operando3 nro-linea]
   (if (or (nil? operando1) (nil? operando2) (nil? operando3)) (dar-error 16 nro-linea)  ; Syntax error
       (case operador
         MID3$ (let [tam (count operando1), ini (dec operando2), fin (+ (dec operando2) operando3)]
                 (cond
                   (or (< operando2 1) (< operando3 0)) (dar-error 53 nro-linea)  ; Illegal quantity error
                   (>= ini tam) ""
                   (>= fin tam) (subs operando1 ini tam)
                   :else (subs operando1 ini fin)))))))

(defn spy
  ([x] (do (prn x) x))
  ([msg x] (do (print msg) (print ": ") (prn x) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A PARTIR DE ESTE PUNTO HAY QUE IMPLEMENTAR LAS FUNCIONES DADAS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; palabra-reservada?: predicado para determinar si un
; identificador es una palabra reservada, por ejemplo:
; user=> (palabra-reservada? 'REM)
; true
; user=> (palabra-reservada? 'SPACE)
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn palabra-reservada? [x]
  (let [palabras-reservadas '(ENV LOAD SAVE RUN EXIT STR$ END LOG IF RETURN ATN PRINT LET READ NEW RESTORE LEN CLEAR EXP GOSUB STEP ON GOTO THEN ASC CHR$ INPUT TO NEXT REM LIST MID$ SIN INT DATA FOR AND OR)]
    (if (some #(= % x) palabras-reservadas) true false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; operador?: predicado para determinar si un identificador es un
; operador, por ejemplo:
; user=> (operador? '+)
; true
; user=> (operador? (symbol "+"))
; true
; user=> (operador? (symbol "%"))
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn operador? [x]
  (let [operadores '(+ - * / = <> < <= > >= AND OR)]
    (if (some #(= % x) operadores) true false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; anular-invalidos: recibe una lista de simbolos y la retorna con
; aquellos que son invalidos reemplazados por nil, por ejemplo:
; user=> (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))
; (IF X nil * Y < 12 THEN LET nil X = 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn anular-invalidos [sentencia] 
  (if (and (seq? sentencia) (or (= 'REM (first sentencia)) (= 'DATA (first sentencia))))
    sentencia
    (map (fn [e] (if (or (= "?" e) (palabra-reservada? e) (operador? e) (number? e) (string? e) (variable-float? e) (variable-integer? e) (variable-string? e) (= "," (str e)) (= ";" (str e)) (= "." (str e)) (= "(" (str e)) (= ")" (str e))) e nil)) sentencia)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; cargar-linea: recibe una linea de codigo y un ambiente y retorna
; el ambiente actualizado, por ejemplo:
; user=> (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])
; [((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
; user=> (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
; [((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
; user=> (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
; [((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
; user=> (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])
; [((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cargar-linea
  ([linea amb] (cargar-linea linea amb () (first amb)))
  ([linea amb vistas pendientes]
   (let [primera-pendiente (first pendientes),
         numero-primera-linea-pendiente (first primera-pendiente),
         numero-linea-nueva (first linea)]
     (cond
       (or (= 0 (count pendientes)) (< numero-linea-nueva numero-primera-linea-pendiente)) (assoc amb 0 (concat (reverse vistas) (conj pendientes linea)))
       (= numero-linea-nueva numero-primera-linea-pendiente) (assoc amb 0 (concat (reverse vistas) (conj (rest pendientes) linea)))
       :else (let [vistas (conj vistas primera-pendiente),
                   pendientes (rest pendientes)]
               (cargar-linea linea amb vistas pendientes))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; expandir-nexts: recibe una lista de sentencias y la devuelve con
; las sentencias NEXT compuestas expresadas como sentencias NEXT
; simples, por ejemplo:
; user=> (def n (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B)))
; #'user/n
; user=> n
; ((PRINT 1) (NEXT A , B))
; user=> (expandir-nexts n)
; ((PRINT 1) (NEXT A) (NEXT B))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn expandir-nexts 
  ([n] (expandir-nexts () n))
  ([acum pendientes]
   (cond
     (= 0 (count pendientes)) (reverse acum)
     (= 'NEXT (ffirst pendientes)) (if (< 3 (count (first pendientes)))
                                     (expandir-nexts (conj acum (take 2 (first pendientes))) (conj (rest pendientes) (conj (rest (rest (rest (first pendientes)))) 'NEXT)))
                                     (expandir-nexts (conj acum (first pendientes)) (rest pendientes)))
     :else (expandir-nexts (conj acum (first pendientes)) (rest pendientes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generar-msg-error genera un mensage de error que usa dar-error
; en prog-ptrs puede recibir un vector con dos posiciones
; representando el nro-linea y cant-sentencias-restantes,
; o un numero que represente el nro-linea
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn generar-msg-error [cod prog-ptrs]
  (let [main-msg (if (string? cod) cod (buscar-mensaje cod)),
        extracted-line-number (if (vector? prog-ptrs) (first prog-ptrs) prog-ptrs)
        location (if (number? extracted-line-number) (str " IN " extracted-line-number) "")]
    (str main-msg location)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dar-error: recibe un error (codigo o mensaje) y el puntero de 
; programa, muestra el error correspondiente {'OR 1, 'AND 2, '* 6, '-u 7, 'MID$ 8}y retorna nil, por
; ejemplo:
; user=> (dar-error 16 [:ejecucion-inmediata 4])
;
; ?SYNTAX  ERRORnil
; user=> (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])
;
; ?ERROR DISK FULLnil
; user=> (dar-error 16 [100 3])
;
; ?SYNTAX  ERROR IN 100nil
; user=> (dar-error "?ERROR DISK FULL" [100 3])
;
; ?ERROR DISK FULL IN 100nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn dar-error [cod prog-ptrs] 
  (print (generar-msg-error cod prog-ptrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; nombre-variable-valido?: Verifica que el argumento sea un 
; nombre de variale valido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn nombre-variable-valido? [v]
  (if (= 1 (count v))
    (Character/isUpperCase (first v))
    (and (= (dec (count v)) (count (first (re-seq #"[A-Z][A-Z0-9]*" (apply str (butlast v))))))
         (or (Character/isUpperCase (last v))
             (Character/isDigit (last v))
             (= \% (last v))
             (= \$ (last v)))
         (not (palabra-reservada? (symbol v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; variable-float?: predicado para determinar si un identificador
; es una variable de punto flotante, por ejemplo:
; user=> (variable-float? 'X)
; true
; user=> (variable-float? 'X%)
; false
; user=> (variable-float? 'X$)
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn variable-float? [x]
  (if (symbol? x)
    (let [v (name x)]
      (and (not= \% (last v)) (not= \$ (last v)) (nombre-variable-valido? v)))
    false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; variable-integer?: predicado para determinar si un identificador
; es una variable entera, por ejemplo:
; user=> (variable-integer? 'X%)
; true
; user=> (variable-integer? 'X)
; false
; user=> (variable-integer? 'X$)
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn variable-integer? [x]
  (if (symbol? x)
    (let [v (name x)]
      (and (= (last v) \%) (nombre-variable-valido? v)))
    false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; variable-string?: predicado para determinar si un identificador
; es una variable de cadena, por ejemplo:
; user=> (variable-string? 'X$)
; true
; user=> (variable-string? 'X)
; false
; user=> (variable-string? 'X%)
; false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn variable-string? [x]
  (if (symbol? x)
    (let [v (name x)]
      (and (= (last v) \$) (nombre-variable-valido? v)))
    false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; contar-sentencias: recibe un numero de linea y un ambiente y
; retorna la cantidad de sentencias que hay en la linea indicada,
; por ejemplo:
; user=> (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
; 2
; user=> (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
; 1
; user=> (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
; 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn contar-sentencias-aux [nro-linea pendientes]
  (cond
    (= 0 (count pendientes)) 0
    (= nro-linea (ffirst pendientes)) (count (expandir-nexts (rest (first pendientes))))
    :else (recur nro-linea (rest pendientes))))

(defn contar-sentencias [nro-linea amb]
  (contar-sentencias-aux nro-linea (first amb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; expandir-solo-nexts-primera-linea: devuelve una lista con la
; primera linea con los nexts expandidos y las demas lineas sin
; expandir sus nexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn expandir-solo-nexts-primera-linea [l]
  (if (empty? l)
    ()
    (let [primera-linea-expandida (conj (as-> (nfirst l) res
                                          (expandir-nexts res))
                                        (ffirst l))] 
      (conj (rest l) primera-linea-expandida))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; buscar-lineas-restantes: recibe un ambiente y retorna la
; representacion intermedia del programa a partir del puntero de
; programa (que indica la linea y cuantas sentencias de la misma
; aun quedan por ejecutar), por ejemplo:
; user=> (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])
; nil
; user=> (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])
; nil
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])
; ((10 (PRINT X) (PRINT Y)) (15 (X = X + 1)) (20 (NEXT I , J)))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])
; ((10 (PRINT Y)) (15 (X = X + 1)) (20 (NEXT I , J)))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])
; ((10) (15 (X = X + 1)) (20 (NEXT I , J)))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])
; ((15 (X = X + 1)) (20 (NEXT I , J)))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])
; ((15) (20 (NEXT I , J)))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
; ((20 (NEXT I) (NEXT J)))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])
; ((20 (NEXT I) (NEXT J)))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])
; ((20 (NEXT J)))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])
; ((20))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])
; ((20))
; user=> (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])
; nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn buscar-lineas-restantes 
  ([amb] (buscar-lineas-restantes (first amb) (first (fnext amb)) (second (fnext amb))))
  ([lineas nro-linea nro-sentencias-restantes]
   (if (keyword? nro-linea)
     nil
     (let [lineas-restantes (drop-while #(not= (first %) nro-linea) lineas)]
       (if (empty? lineas-restantes)
         nil
         (as-> lineas-restantes l
           (expandir-solo-nexts-primera-linea l)
           (let [pendientes-primera-linea-restante (conj (take-last (max nro-sentencias-restantes 0) (nfirst l)) (ffirst l))]
             (if (nil? pendientes-primera-linea-restante)
               (rest l)
               (conj (rest l) pendientes-primera-linea-restante)))
           (if (empty? l) (list (list nro-linea)) l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; continuar-linea: implementa la sentencia RETURN, retornando una
; dupla (un vector) con un resultado (usado luego por
; evaluar-linea) y un ambiente actualizado con el nuevo valor del
; puntero de programa, por ejemplo:
; user=> (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
; 
; ?RETURN WITHOUT GOSUB ERROR IN 20[nil [((10 (PRINT X)) (15 (X = X + 1)) (20 (NEXT I , J))) [20 3] [] [] [] 0 {}]]
; user=> (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])
; [:omitir-restante [((10 (PRINT X)) (15 (GOSUB 100) (X = X + 1)) (20 (NEXT I , J))) [15 1] [] [] [] 0 {}]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn continuar-linea [amb]
  (if (empty? (amb 2))
    (vector (dar-error 22 (amb 1)) amb)
    (let [amb-con-puntero-actualizado (assoc amb 1 (vector (ffirst (amb 2)) (dec (second (first (amb 2))))))
          amb-completamente-actualizado (assoc amb-con-puntero-actualizado 2 (into [] (rest (amb-con-puntero-actualizado 2))))]
      (vector :omitir-restante amb-completamente-actualizado))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; recibe una sentencia DATA y retorna sus valores embebidos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn extraer-data-sentencia [sentencia]
  (map #(if (symbol? %) (name %) %) 
       (filter #(not= (symbol ",") %) (rest sentencia))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; recibe una linea y retorna los valores embebidos en las 
; sentencias DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn extraer-data-linea [linea]
  (reduce #(if (= 'DATA (first %2)) 
             (concat %1 (extraer-data-sentencia %2))
             %1)
          ()
          (take-while #(not= (first %) 'REM) (rest linea))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; extraer-data: recibe la representación intermedia de un programa
; y retorna una lista con todos los valores embebidos en las
; sentencias DATA, por ejemplo:
; user=> (extraer-data '(()))
; ()
; user=> (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))
; ("HOLA" "MUNDO" 10 20)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn extraer-data [prg]
  (reduce #(concat %1 (extraer-data-linea %2)) () prg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ejecutar-asignacion: recibe una asignacion y un ambiente, y
; retorna el ambiente actualizado al efectuar la asignacion, por
; ejemplo:
; user=> (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])
; [((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]
; user=> (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
; [((10 (PRINT X))) [10 1] [] [] [] 0 {X 5}]
; user=> (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])
; [((10 (PRINT X))) [10 1] [] [] [] 0 {X 3}]
; user=> (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])
; [((10 (PRINT X))) [10 1] [] [] [] 0 {X$ "HOLA MUNDO"}]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; usa la forma identificador = <expresion>
;; por lo que deberia poder implemntar una asignacion del tipo 
;; identificador = <numero> sin mayores complicaciones
;; sin embargo un X = X + 1 ya seria mas complejo
;; como implementar este segundo caso, haciendo caso a los
;; ordenes de precedencia y mostrando los errores apropiados?


; primero hacer patio de maniobras de Dijkstra y luego usar calcular-rpn
; en vez de hacerlo manual creo que puedo usar calcular-expresion

(defn ejecutar-asignacion [sentencia amb]
  (assoc amb 6 (assoc (last amb) (first sentencia) (calcular-expresion (drop 2 sentencia) amb))))

; (= ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; preprocesar-expresion: recibe una expresion y la retorna con
; las variables reemplazadas por sus valores y el punto por el
; cero, por ejemplo:
; user=> (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])
; ("HOLA" + " MUNDO" + "")
; user=> (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])
; (5 + 0 / 2 * 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn preprocesar-expresion [expr amb]
  (if (= '(nil) expr)
    nil
    (map
     (fn [e]
       (cond
         (= '. e) 0
         (or (string? e) (number? e)) e
         (and (nombre-variable-valido? (name e)) (contains? (last amb) e)) (get (last amb) e)
         (and (not (operador? e)) (not (palabra-reservada? e)) (nombre-variable-valido? (name e))) (cond
                                                                                                     (variable-float? e) 0
                                                                                                     (variable-integer? e) 0
                                                                                                     (variable-string? e) "")
         :else e))
     expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; desambiguar: recibe un expresion y la retorna sin los + unarios,
; con los - unarios reemplazados por -u y los MID$ ternarios
; reemplazados por MID3$, por ejemplo: 
; user=> (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))
; (-u 2 * ( -u 3 + 5 - ( 2 / 7 ) ))
; user=> (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))
; (MID$ ( 1 , 2 ))
; user=> (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))
; (MID3$ ( 1 , 2 , 3 ))
; user=> (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))
; (MID3$ ( 1 , -u 2 + K , 3 ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn desambiguar
  [expr] 
  (seq (desambiguar-comas (desambiguar-mas-menos (desambiguar-mid expr)))))                                                                                                                                                            
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; precedencia: recibe un token y retorna el valor de su
; precedencia, por ejemplo:
; user=> (precedencia 'OR)
; 1
; user=> (precedencia 'AND)
; 2
; user=> (precedencia '*)
; 6
; user=> (precedencia '-u)
; 7
; user=> (precedencia 'MID$)
; 8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn precedencia [token]
  (let [jerarquia {(symbol ",") 0,
                   'OR 1,
                   'AND 2,
                   'NOT 3,
                   '<> 4,
                   '< 4,
                   '<= 4,
                   '> 4,
                   '>= 4,
                   '= 4,
                   '+ 5,
                   '- 5,
                   '* 6,
                   '/ 6,
                   '-u 7,
                   'ATN 8,
                   'INT 8,
                   'SIN 8,
                   'EXP 8,
                   'LOG 8,
                   'LEN 8,
                   'MID$ 8,
                   'MID3$ 8,
                   'ASC 8,
                   'CHR$ 8,
                   'STR$ 8}]
    (jerarquia token)))

;; MID$ recibe una cadena y una posicion que es desde donde la retorna, y un
;; tercer argumento que es la cantidad de caracteres que lee
;; ej: MID$("HOLA MUNDO",6,2)
;; MU
;; internamente si se pasan 3 argumentos se usa MID3$, pero el usuario no puede
;; llamarla directamente

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; aridad: recibe un token y retorna el valor de su aridad, por
; ejemplo:
; user=> (aridad 'THEN)
; 0
; user=> (aridad 'SIN)
; 1
; user=> (aridad '*)
; 2
; user=> (aridad 'MID$)
; 2
; user=> (aridad 'MID3$)
; 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aridad [token]
  (if (not (symbol? token))
    0
    (let [aridades {:ASC 1,
                    :CHR$ 1,
                    :<= 2,
                    :* 2,
                    :> 2,
                    :AND 2,
                    :EXP 1,
                    :-u 1,
                    :- 2,
                    :LEN 1,
                    :OR 2,
                    :/ 2,
                    :>= 2,
                    :<> 2,
                    :ATN 1,
                    :+ 2,
                    :LOG 1,
                    :STR$ 1,
                    :INT 1,
                    := 2,
                    :< 2,
                    :SIN 1,
                    :MID$ 2,
                    :MID3$ 3},
          valor ((keyword (name token)) aridades)]
      (if (nil? valor) 0 valor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; eliminar-cero-decimal: recibe un numero y lo retorna sin ceros
; decimales no significativos, por ejemplo: 
; user=> (eliminar-cero-decimal 1.5)
; 1.5
; user=> (eliminar-cero-decimal 1.50)
; 1.5
; user=> (eliminar-cero-decimal 1.0)
; 1
; user=> (eliminar-cero-decimal 'A)
; A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eliminar-cero-decimal [n]
  (cond 
    (symbol? n) n
    (int? n) n
    (string? n) n
    (= (* 1.0 (int n)) n) (int n) 
    (rational? n) (double n)
    :else n))
;; (defn eliminar-cero-decimal [n]
;;   (if (= 1.0 n) 1 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; eliminar-cero-entero: recibe un simbolo y lo retorna convertido
; en cadena, omitiendo para los numeros del intervalo (-1..1) el
; cero a la izquierda del punto, por ejemplo:
; user=> (eliminar-cero-entero nil)
; nil
; user=> (eliminar-cero-entero 'A)
; "A"
; user=> (eliminar-cero-entero 0)
; " 0"
; user=> (eliminar-cero-entero 1.5)
; " 1.5"
; user=> (eliminar-cero-entero 1)
; " 1"
; user=> (eliminar-cero-entero -1)
; "-1"
; user=> (eliminar-cero-entero -1.5)
; "-1.5"
; user=> (eliminar-cero-entero 0.5)
; " .5"
; user=> (eliminar-cero-entero -0.5)
; "-.5"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eliminar-cero-entero [n]
  (cond
    (number? n) (cond
                  (zero? n) " 0"
                  (and (pos? n) (< n 1)) (let [parte-decimal (- n (int n))] (str " ." (apply str (nnext (str parte-decimal)))))
                  (and (neg? n) (> n -1)) (let [parte-decimal (* -1 (- n (int n)))] (str "-." (apply str (nnext (str parte-decimal)))))
                  (<= n -1) (str n)
                  :else (str " " n))
    (= n nil) nil
    :else (str n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; flatten-primer-nivel: remueve los parentesis al primer nivel
; unicamente de una secuencia util para preparar las lineas a
; mostrar por LIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn flatten-primer-nivel
  ([arg] (flatten-primer-nivel () arg))
  ([acum arg] (cond
                (empty? arg) (reverse acum)
                (seq? (first arg)) (flatten-primer-nivel (reduce (fn [procesado e] (conj procesado e)) acum (first arg)) (rest arg))
                :else (flatten-primer-nivel (conj acum (first arg)) (rest arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; insertar-separadores-sentencias: inserta separadores entre
; sentencias de una linea
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn insertar-separadores-sentencias 
  ([sentencias] (insertar-separadores-sentencias (reverse (take 2 sentencias)) (drop 2 sentencias)))
  ([acum pend] (cond
                 (empty? pend) (reverse acum)
                 :else (insertar-separadores-sentencias (conj (conj acum (symbol ":")) (first pend)) (rest pend)))))

true
