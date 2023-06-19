(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.interprete :refer :all]))

;;(defn generate-operador?-tests [lst]
;;  (map (fn [kw] `(is (= true (operador? '~kw)))) lst))

(deftest palabra-reservada?-test
  (testing "palabra-reservada?"
    (is (= true (palabra-reservada? 'ENV)))
    (is (= true (palabra-reservada? 'LOAD)))
    (is (= true (palabra-reservada? 'SAVE)))
    (is (= true (palabra-reservada? 'RUN)))
    (is (= true (palabra-reservada? 'EXIT)))
    (is (= true (palabra-reservada? 'STR$)))
    (is (= true (palabra-reservada? 'END)))
    (is (= true (palabra-reservada? 'LOG)))
    (is (= true (palabra-reservada? 'IF)))
    (is (= true (palabra-reservada? 'RETURN)))
    (is (= true (palabra-reservada? 'ATN)))
    (is (= true (palabra-reservada? 'PRINT)))
    (is (= true (palabra-reservada? 'LET)))
    (is (= true (palabra-reservada? 'READ)))
    (is (= true (palabra-reservada? 'NEW)))
    (is (= true (palabra-reservada? 'RESTORE)))
    (is (= true (palabra-reservada? 'LEN)))
    (is (= true (palabra-reservada? 'CLEAR)))
    (is (= true (palabra-reservada? 'EXP)))
    (is (= true (palabra-reservada? 'GOSUB)))
    (is (= true (palabra-reservada? 'STEP)))
    (is (= true (palabra-reservada? 'ON)))
    (is (= true (palabra-reservada? 'GOTO)))
    (is (= true (palabra-reservada? 'THEN)))
    (is (= true (palabra-reservada? 'ASC)))
    (is (= true (palabra-reservada? 'CHR$)))
    (is (= true (palabra-reservada? 'INPUT)))
    (is (= true (palabra-reservada? 'TO)))
    (is (= true (palabra-reservada? 'NEXT)))
    (is (= true (palabra-reservada? 'REM)))
    (is (= true (palabra-reservada? 'LIST)))
    (is (= true (palabra-reservada? 'MID$)))
    (is (= true (palabra-reservada? 'SIN)))
    (is (= true (palabra-reservada? 'INT)))
    (is (= true (palabra-reservada? 'DATA)))
    (is (= true (palabra-reservada? 'FOR)))
    (is (= true (palabra-reservada? 'AND)))
    (is (= true (palabra-reservada? 'OR)))
    (is (= false (palabra-reservada? 'SPACE)))))

(deftest operador?-tests
  (testing "operador?"
    (is (= true (operador? '+)))
    (is (= true (operador? '-)))
    (is (= true (operador? '*)))
    (is (= true (operador? '/)))
    (is (= true (operador? '=)))
    (is (= true (operador? '<>)))
    (is (= true (operador? '<)))
    (is (= true (operador? '<=)))
    (is (= true (operador? '>)))
    (is (= true (operador? '>=)))
    (is (= true (operador? 'AND)))
    (is (= true (operador? 'OR)))
    (is (= false (operador? 'SPACE)))))

(deftest anular-invalidos-tests
  (testing "anular-invalidos"
    (is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))
    (is (= '(PRINT 5) (anular-invalidos '(PRINT 5))))
    (is (= '(LET P = .) (anular-invalidos '(LET P = .))))
    (is (= '(PRINT "HOLA") (anular-invalidos '(PRINT "HOLA"))))
    (is (= (list 'LET 'X '= 'LEN (symbol "(") "HOLA" (symbol ")")) (anular-invalidos (list 'LET 'X '= 'LEN (symbol "(") "HOLA" (symbol ")")))))
    (is (= '(REM COMENTARIO) (anular-invalidos '(REM COMENTARIO))))
    (is (= (list 'REM (symbol "ESTE COMENTARIO")) (anular-invalidos (list 'REM (symbol "ESTE COMENTARIO")))))))

(deftest variable-float?-tests
  (testing "variable-float?"
    (is (= true (variable-float? 'X)))
    (is (= false (variable-float? 'X%)))
    (is (= false (variable-float? 'X$)))
    (is (= true (variable-float? 'X1)))))

(deftest variable-integer?-tests
  (testing "variable-integer?"
    (is (= true (variable-integer? 'X%)))
    (is (= false (variable-integer? 'X)))
    (is (= false (variable-integer? 'X$)))
    (is (= false (variable-integer? 'X1)))))

(deftest variable-string?-tests
  (testing "variable-string?"
    (is (= true (variable-string? 'X$)))
    (is (= false (variable-string? 'X)))
    (is (= false (variable-string? 'X%)))
    (is (= false (variable-string? 'X1)))))

(deftest precedencia-tests
  (testing "precedencia"
    (is (= 1 (precedencia 'OR)))
    (is (= 2 (precedencia 'AND)))
    (is (= 3 (precedencia 'NOT)))
    (is (= 4 (precedencia '<>)))
    (is (= 4 (precedencia '<)))
    (is (= 4 (precedencia '<=)))
    (is (= 4 (precedencia '>)))
    (is (= 4 (precedencia '>=)))
    (is (= 4 (precedencia '=)))
    (is (= 5 (precedencia '+)))
    (is (= 5 (precedencia '-)))
    (is (= 6 (precedencia '*)))
    (is (= 6 (precedencia '/)))
    (is (= 7 (precedencia '-u)))
    (is (= 8 (precedencia 'MID$)))
    (is (= 8 (precedencia 'ATN)))
    (is (= 8 (precedencia 'INT)))
    (is (= 8 (precedencia 'SIN)))
    (is (= 8 (precedencia 'EXP)))
    (is (= 8 (precedencia 'LOG)))
    (is (= 8 (precedencia 'LEN)))
    (is (= 8 (precedencia 'MID$)))
    (is (= 8 (precedencia 'ASC)))
    (is (= 8 (precedencia 'CHR$)))
    (is (= 8 (precedencia 'STR$)))))

(deftest eliminar-cero-decimal-tests
  (testing "eliminar-cero-decimal"
    (is (= 1.5 (eliminar-cero-decimal 1.5)))
    (is (= 1.5 (eliminar-cero-decimal 1.50)))
    (is (= 1 (eliminar-cero-decimal 1.0)))
    (is (= 'A (eliminar-cero-decimal 'A)))))

(deftest eliminar-cero-entero-tests
  (testing "eliminar-cero-entero"
    (is (= nil (eliminar-cero-entero nil)))
    (is (= "A" (eliminar-cero-entero 'A)))
    (is (= " 0" (eliminar-cero-entero 0)))
    (is (= " 1.5" (eliminar-cero-entero 1.5)))
    (is (= " 1" (eliminar-cero-entero 1)))
    (is (= "-1" (eliminar-cero-entero -1)))
    (is (= "-1.5" (eliminar-cero-entero -1.5)))
    (is (= " .5" (eliminar-cero-entero 0.5)))
    (is (= "-.5" (eliminar-cero-entero -0.5)))))

(deftest aridad-tests
  (testing "aridad"
    (is (= 0 (aridad 5)))
    (is (= 0 (aridad 'THEN)))
    (is (= 1 (aridad 'ATN)))
    (is (= 1 (aridad 'INT)))
    (is (= 1 (aridad 'SIN)))
    (is (= 1 (aridad 'EXP)))
    (is (= 1 (aridad 'LOG)))
    (is (= 2 (aridad 'MID$)))
    (is (= 3 (aridad 'MID3$)))
    (is (= 1 (aridad 'ASC)))
    (is (= 1 (aridad 'CHR$)))
    (is (= 1 (aridad 'STR$)))
    (is (= 1 (aridad 'LEN)))
    (is (= 2 (aridad '+)))
    (is (= 1 (aridad '-u)))
    (is (= 2 (aridad '-)))
    (is (= 2 (aridad '*)))
    (is (= 2 (aridad '/)))
    (is (= 2 (aridad '=)))
    (is (= 2 (aridad '<>)))
    (is (= 2 (aridad '<)))
    (is (= 2 (aridad '<=)))
    (is (= 2 (aridad '>)))
    (is (= 2 (aridad '>=)))
    (is (= 2 (aridad 'AND)))
    (is (= 2 (aridad 'OR)))))

(deftest nombre-variable-valido?-tests
  (testing "nombre-variable-valido?"
    (is (= true (nombre-variable-valido? "X")))
    (is (= true (nombre-variable-valido? "X%")))
    (is (= true (nombre-variable-valido? "X$")))
    (is (= true (nombre-variable-valido? "X1")))
    (is (= false (nombre-variable-valido? "X!")))
    (is (= false (nombre-variable-valido? "X!A")))))

(deftest cargar-linea-tests
  (testing "cargar-linea"
    (is (= ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
           (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((10 (PRINT X)) (15 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
           (cargar-linea '(10 (PRINT X)) ['((15 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((15 (X = X - 1))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
           (cargar-linea '(15 (X = X - 1)) ['((15 (X = X + 1))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
           (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
           (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
           (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (= ['((10 (LET X = 1)) (20 (PRINT X)) (30 (END))) [:ejecucion-inmediata 0] [] [] [] 0 {}]
           (cargar-linea '(30 (END)) ['((10 (LET X = 1)) (20 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))))

(deftest generar-msg-error-tests
  (testing "generar-msg-error"
    (is (= "?SYNTAX  ERROR" (generar-msg-error 16 [:ejecucion-inmediata 4])))
    (is (= "?ERROR DISK FULL" (generar-msg-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])))
    (is (= "?SYNTAX  ERROR IN 100" (generar-msg-error 16 [100 3])))
    (is (= "?ERROR DISK FULL IN 100" (generar-msg-error "?ERROR DISK FULL" [100 3])))
    (is (= "?ERROR DISK FULL IN 100" (generar-msg-error "?ERROR DISK FULL" 100)))))

(deftest expandir-nexts-tests
  (testing "expandir-nexts"
    (let [n '((PRINT X) (NEXT A) (PRINT Y))]
      (is (= '((PRINT X) (NEXT A) (PRINT Y)) (expandir-nexts n))))
    (let [n (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B))]
      (is (= '((PRINT 1) (NEXT A) (NEXT B)) (expandir-nexts n))))))

(deftest desambiguar-tests
  (testing "desambiguar"
    (is (= (list '-u 2 '* (symbol "(") '-u 3 '+ 5 '- (symbol "(") 2 '/ 7 (symbol ")") (symbol ")")) (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")")))))
    (is (= (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")))))
    (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))))
    (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 '+ 'K (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))))
    (is (= (list "HOLA" + "MUNDO") (desambiguar (list "HOLA" + "MUNDO"))))))

(deftest contar-sentencias-tests
  (testing "contar-sentencias"
    (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))))

(deftest preprocesar-expresion-tests
  (testing "preprocesar-expresion"
    (is (= '("HOLA" + " MUNDO" + "") (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
    (is (= '(5 + 0 / 2 * 0) (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))
    (is (= '(PRINT) (preprocesar-expresion '(PRINT) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))
    (is (= '("HOLA ") (preprocesar-expresion '("HOLA ") ['() [:ejecucion-inmediata 0] [] [] [] 0 '{X 5}])))))

(deftest expandir-solo-nexts-primera-linea-tests
  (testing "expandir-solo-nexts-primera-linea-tests"
    (is (= () (expandir-solo-nexts-primera-linea ())))
    (is (= (list '(10 (PRINT Y) (NEXT X) (NEXT Y)) (list 20 '(PRINT X) (list 'NEXT 'X (symbol ",") 'Y))) (expandir-solo-nexts-primera-linea (list (list 10 '(PRINT Y) (list 'NEXT 'X (symbol ",") 'Y)) (list 20 '(PRINT X) (list 'NEXT 'X (symbol ",") 'Y))))))))

(deftest buscar-lineas-restantes-tests
  (testing "buscar-lineas-restantes"
    (is (nil? (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
    (is (nil? (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])))
    (is (= (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}])))
    (is (= (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
    (is (= (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])))
    (is (= (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))
    (is (= (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))
    (is (= (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])))
    (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
    (is (= '((20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])))
    (is (= '((20 (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])))
    (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])))
    (is (= '((20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])))
    (is (nil? (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])))))

(deftest ejecutar-asignacion-tests
  (testing "ejecutar-asignacion"
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])))
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 3}] (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
    (is (= ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA MUNDO"}] (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))))

(deftest extraer-data-sentencia-tests
  (testing "extraer-data-sentencia"
    (is (= '(1 2 3) (extraer-data-sentencia (list 'DATA 1 (symbol ",") 2 (symbol ",") 3))))
    (is (= '(1 "HOLA" 3) (extraer-data-sentencia (list 'DATA 1 (symbol ",") 'HOLA (symbol ",") 3))))))

(deftest extraer-data-linea-tests
  (testing "extraer-data-linea"
    (is (= '() (extraer-data-linea '(10 (PRINT X) (PRINT X)))))
    (is (= '() (extraer-data-linea '(10 (PRINT X) (REM ESTE NO) (DATA 1)))))
    (is (= '(1 2 "HOLA") (extraer-data-linea (list 10 (list 'DATA 1 (symbol ",") 2) '(DATA HOLA)))))))

(deftest extraer-data-tests
  (testing "extraer-data"
    (is (= () (extraer-data '(()))))
    (is (= '("HOLA" "MUNDO" 10 20) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))))))
(type ())
(type (extraer-data '(())))
(deftest continuar-linea-tests
  (testing "continuar-linea"
    (is (= [nil [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
    (is (= [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])))
    (is (= [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [[20 1]] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2] [20 1]] [] [] 0 {}])))))

(deftest aplicar-tests
  (testing "aplicar"
    (testing "-u"
      (testing "debe retornar el numero negativo"
        (is (= -2 (aplicar '-u 2 1)))))
    (testing "LEN"
      (testing "debe retornar la longitud de una cadena"
        (is (= 4 (aplicar 'LEN "HOLA" 1)))))
    (testing "STR$"
      (testing "debe retornar nil si no se le pasa un numero"
        (is (nil? (aplicar 'STR$ "HOLA" 1))))
      (testing "debe retornar un numero convertido en cadena"
        (is (= " 3" (aplicar 'STR$ 3 1)))))
    (testing "CHR$"
      (testing "debe retornar nil si el numero es menor a 0"
        (is (nil? (aplicar 'CHR$ -1 1))))
      (testing "debe retornar nil si el numero es mayor a 255"
        (is (nil? (aplicar 'CHR$ 256 1))))
      (testing "debe retornar el caracter de un codigo ASCII dado"
        (is (= "A" (aplicar 'CHR$ 65 1)))))
    (testing "INT"
      (testing "debe retornar la parte entera de un numero"
        (is (= 3 (aplicar 'INT 3.4 1))))
      (testing "debe retornar nil si no se le pasa un numero"
        (is (nil? (aplicar 'INT "HOLA" 1)))))
    (testing "SIN"
      (testing "debe retornar nil si no se le pasa un numero"
        (is (nil? (aplicar 'SIN "HOLA" 1))))
      (testing "debe retornar el seno de un numero"
        (is (= (. Math sin 1) (aplicar 'SIN 1 1)))))
    (testing "LOG"
      (testing "debe retornar nil si no se le pasa un numero"
        (is (nil? (aplicar 'LOG "HOLA" 1))))
      (testing "debe retornar el logaritmo natural del argumento"
        (is (= (. Math log 3) (aplicar 'LOG 3 1)))))
    (testing "EXP"
      (testing "debe retornar nil si no se le pasa un numero"
        (is (nil? (aplicar 'EXP "HOLA" 1))))
      (testing "debe retornar la constante E elevada al argumento"
        (is (= (. Math exp 2) (aplicar 'EXP 2 1)))))
    (testing "ASC"
      (testing "debe retornar nil si no se le pasa una cadena de caracteres"
        (is (nil? (aplicar 'ASC 1 1))))
      (testing "debe retornar el numero ASCII del primer caracter de la inicial de una cadena"
        (is (= 72 (aplicar 'ASC "HOLA" 1)))))
    (testing "ATN"
      (testing "debe retornar nil si no se le pasa un numero"
        (is (nil? (aplicar 'ATN "HOLA" 1))))
      (testing "debe retornar la arcotangente de un numoer"
        (is (= (. Math atan 3) (aplicar 'ATN 3 1)))))
    (testing "+"
      (testing "debe retornar la suma de dos numeros"
        (is (= 5 (aplicar '+ 2 3 1))))
      (testing "debe concatenar dos cadenas"
        (is (= "HOLA MUNDO" (aplicar '+ "HOLA" " MUNDO" 1)))))
    (testing "-"
      (testing "debe retornar la resta de dos numeros"
        (is (= 4 (aplicar '- 6 2 1)))))
    (testing "*"
      (testing "debe retornar la multiplicacion de dos numeros"
        (is (= 6 (aplicar '* 2 3 1)))))
    (testing "/"
      (testing "debe retornar nil si el divisor es 0"
        (is (= nil (aplicar '/ 2 0 1))))
      (testing "debe retornar la division de dos numeros"
        (is (= 3 (aplicar '/ 6 2 1)))))
    (testing "AND"
      (testing "debe retornar -1 si sus dos valores son verdaderos"
        (is (= -1 (aplicar 'AND 2 3 1))))
      (testing "debe retornar 0 si su primer valor es falso"
        (is (= 0 (aplicar 'AND 0 3 1))))
      (testing "debe retornar 0 si su segundo valor es falso"
        (is (= 0 (aplicar 'AND 3 0 1))))
      (testing "debe retornar 0 si sus dos valores son falsos"
        (is (= 0 (aplicar 'AND 0 0 1)))))
    (testing "OR"
      (testing "debe retornar -1 si sus dos valores son verdaderos"
        (is (= -1 (aplicar 'OR 2 3 1))))
      (testing "debe retornar -1 si su primer valor es verdadero"
        (is (= -1 (aplicar 'OR 3 0 1))))
      (testing "debe retornar -1 si su segundo valor es verdadero"
        (is (= -1 (aplicar 'OR 0 3 1))))
      (testing "debe retornar 0 si sus dos valores son falsos"
        (is (= 0 (aplicar 'OR 0 0 1)))))
    (testing "<>"
      (testing "debe retornar 0 si dos cadenas son iguales"
        (is (= 0 (aplicar '<> "HOLA" "HOLA" 1))))
      (testing "debe retornar 0 si dos numeros son iguales"
        (is (= 0 (aplicar '<> 2 2 1))))
      (testing "debe retornar -1 si dos cadenas son distintas"
        (is (= -1 (aplicar '<> "HOLA" "CHAU" 1))))
      (testing "debe retornar -1 si dos numeros son distintos"
        (is (= -1 (aplicar '<> 3 2 1)))))
    (testing "<"
      (testing "debe retornar nil si al menos uno de sus argumentos es una cadena"
        (is (nil? (aplicar '< "HOLA" 2 1))))
      (testing "debe retornar -1 si el primer numero es menor que el segundo"
        (is (= -1 (aplicar '< 1 2 1))))
      (testing "debe retornar 0 si el primer numero es igual que el segundo"
        (is (= 0 (aplicar '< 2 2 1))))
      (testing "debe retornar 0 si el primer numero es mayor que el segundo"
        (is (= 0 (aplicar '< 2 1 1)))))
    (testing "<="
      (testing "debe retornar nil si al menos uno de sus argumentos es una cadena"
        (is (nil? (aplicar '<= "HOLA" 2 1))))
      (testing "debe retornar -1 si el primer numero es menor que el segundo"
        (is (= -1 (aplicar '<= 1 2 1))))
      (testing "debe retornar -1 si el primer numero es igual que el segundo"
        (is (= -1 (aplicar '<= 2 2 1))))
      (testing "debe retornar 0 si el primer numero es mayor que el segundo"
        (is (= 0 (aplicar '<= 2 1 1)))))
    (testing ">"
      (testing "debe retornar nil si al menos uno de sus argumentos es una cadena"
        (is (nil? (aplicar '> "HOLA" 2 1))))
      (testing "debe retornar 0 si el primer numero es menor que el segundo"
        (is (= 0 (aplicar '> 1 2 1))))
      (testing "debe retornar 0 si el primer numero es igual que el segundo"
        (is (= 0 (aplicar '> 2 2 1))))
      (testing "debe retornar -1 si el primer numero es mayor que el segundo"
        (is (= -1 (aplicar '> 2 1 1)))))
    (testing ">="
      (testing "debe retornar nil si al menos uno de sus argumentos es una cadena"
        (is (nil? (aplicar '>= "HOLA" 2 1))))
      (testing "debe retornar 0 si el primer numero es menor que el segundo"
        (is (= 0 (aplicar '>= 1 2 1))))
      (testing "debe retornar -1 si el primer numero es igual que el segundo"
        (is (= -1 (aplicar '>= 2 2 1))))
      (testing "debe retornar -1 si el primer numero es mayor que el segundo"
        (is (= -1 (aplicar '>= 2 1 1)))))
    (testing "="
      (testing "debe retornar -1 si dos cadenas son iguales"
        (is (= -1 (aplicar '= "HOLA" "HOLA" 1))))
      (testing "debe retornar -1 si dos numeros son iguales"
        (is (= -1 (aplicar '= 2 2 1))))
      (testing "debe retornar 0 si dos cadenas son distintas"
        (is (= 0 (aplicar '= "HOLA" "CHAU" 1))))
      (testing "debe retornar 0 si dos numeros son distintos"
        (is (= 0 (aplicar '= 3 2 1)))))
    (testing "MID$"
      (testing "debe retornar nil si el numero de caracter es menor a 1"
        (is (nil? (aplicar 'MID$ "HOLA" 0 1))))
      (testing "debe retornar una subcadena desde el numero de caracter indicado"
        (is (= "OLA" (aplicar 'MID$ "HOLA" 2 1)))))
    (testing "MID3$"
      (testing "debe retornar nil si el numero de caracter inicial es menor a 1"
        (is (nil? (aplicar 'MID3$ "HOLA" 0 1 1))))
      (testing "debe retornar nil si el numero de caracteres a retornar es menor a 0"
        (is (nil? (aplicar 'MID3$ "HOLA" 1 -1 1))))
      (testing "debe retornal una subcadena desde el numero de caracter inicial indicado y con la cantidad de caracteres indicada"
        (is (= "OL" (aplicar 'MID3$ "HOLA" 2 2 1)))))))

(deftest evaluar-tests
  (testing "evaluar"
    (testing "DATA"
      (testing "debe retornar una dupla con :omitir-restante y el ambiente"
        (is (= [:omitir-restante ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]]
               (evaluar '(DATA 10) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}])))))
    (testing "END"
      (testing "debe retornar una dupla con :omitir-restante y un ambiente que solo conserve las sentencias"
        (is (= [:omitir-restante ['(10 (PRINT X) (PRINT Y)) [:ejecucion-inmediata 0] [] [] [] 0 {}]]
               (evaluar '(END) ['(10 (PRINT X) (PRINT Y)) [10 1] [] [] [] 0 '{X 3, Y 4}])))))
    (testing "READ"
      (testing "debe retornar una dupla con :error-parcial y el ambiente si no hay mas datos"
        (is (= [:error-parcial ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]]
               (evaluar '(READ X) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]))))
      (testing "debe retornar una dupla con :sin-errores y el ambiente con el puntero actualizado"
        (is (= [:sin-errores ['() [:ejecucion-inmediata 0] [] [] [10] 1 '{X 10}]]
               (evaluar '(READ X) ['() [:ejecucion-inmediata 0] [] [] [10] 0 '{X 4}])))))
    (testing "RESTORE"
      (testing "debe retornar una dupla con :sin-errores y el ambiente con data-ptr = 0"
        (is (= [:sin-errores ['() [:ejecucion-inmediata 0] [] [] [10 20 30 40] 0 '{X 30}]]
               (evaluar '(RESTORE) ['() [:ejecucion-inmediata 0] [] [] [10 20 30 40] 3 '{X 30}])))))
    (testing "CLEAR"
      (testing "debe retornar una dupla con :sin-errores y el ambiente con var-mem vacio"
        (is (= [:sin-errores ['() [:ejecucion-inmediata 0] [] [] [] 0 '{}]]
               (evaluar '(CLEAR) ['() [:ejecucion-inmediata 0] [] [] [] 0 '{X 30, Y 40}])))))
    (testing "LIST"
      (testing "debe retornar una dupla con :sin-errores y el ambiente"
        (is (= [:sin-errores ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]]
               (evaluar '(LIST) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}])))))
    (testing "LET"
      (testing "debe retornar una dupla con :error-parcial y el ambiente si la sentencia no tiene como tercer elemento un simbolo ="
        (is (= [:error-parcial ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]]
               (evaluar '(LET X 4) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]))))
      (testing "debe retornar una dupla con :error-parcial y el ambiente si la expresion esta mal formada"
        (is (= [:error-parcial ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]]
               (evaluar '(LET X = PRINT) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]))))
      (testing "debe retornar una dupla con :sin-errores y el ambiente con var-mem actualizado si la variable no existia"
        (is (= [:sin-errores ['() [:ejecucion-inmediata 0] [] [] [] 0 '{X 5}]]
               (evaluar '(LET X = 5) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]))))
      (testing "debe retornar una dupla con :sin-errores y el ambiente con var-mem actualizado si la variable ya existia"
        (is (= [:sin-errores ['() [:ejecucion-inmediata 0] [] [] [] 0 '{X 5}]]
               (evaluar '(LET X = 5) ['() [:ejecucion-inmediata 0] [] [] [] 0 '{X 4}]))))
      (testing "debe retornar una dupla con :sin-errores y el ambiente con var-mem actualizado con valor 0 para la variable nueva si es igual a ."
        (is (= [:sin-errores ['() [:ejecucion-inmediata 0] [] [] [] 0 '{P 0}]]
               (evaluar '(LET P = .) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}])))))
    (testing "PRINT"
      (testing "debe retornar una dupla con :sin-errores y el ambiente si no se le pasan argumentos"
        (is (= [:sin-errores ['() [:ejecucion-inmediata 0] [] [] [] 0 {}]]
               (evaluar '(PRINT) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}])))))))

(deftest flatten-primer-nivel-tests
  (testing "flatten-primer-nivel"
    (is (= '(10 PRINT X PRINT Y) (flatten-primer-nivel '(10 (PRINT X) (PRINT Y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ERROR 1: print con una funcion no funciona

; ej: PRINT LEN("HOLA")

; EXPLICACION DEL PROBLEMA

; El codigo escrito por el profe llama a calcular-expresion
; pasandole solo una parte de la expresion

; PASOS PARA DEPURAR

; (evaluar (list 'PRINT 'LEN (symbol "(") "HOLA" (symbol ")")) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}])
; (evaluar (list 'PRINT (symbol "(") 'LEN (symbol "(") "HOLA" (symbol ")") (symbol ")")) ['() [:ejecucion-inmediata 0] [] [] [] 0 {}])

; colocar spies en la funcion imprimir para expresiones, amb,
; lista-expr y el otro amb (para su segunda definicion). Se
; pueden agregar mas spies en cada llamado, especialmente 
; el segundo

; deberia llamarse desdeimprimir a calclular expresion de una manera similar a esta
; (calcular-expresion (list 'LEN (symbol "(") "HOLA" (symbol ")")) [() [] [] [] [] 0 {}])
;; => 4

; identifica variable? a LEN como variable, lo cual esta mal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
