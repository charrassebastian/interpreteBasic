(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.main :refer :all]))

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
    (is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))))

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
    (is (= 8 (precedencia 'MID$)))))

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
           (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))))

(deftest generar-msg-error-tests
  (testing "generar-msg-error"
    (is (= "?SYNTAX  ERROR" (generar-msg-error 16 [:ejecucion-inmediata 4])))
    (is (= "?ERROR DISK FULL" (generar-msg-error "?ERROR DISK FULL" [:ejecucion-inmediata 4])))
    (is (= "?SYNTAX  ERROR IN 100" (generar-msg-error 16 [100 3])))
    (is (= "?ERROR DISK FULL IN 100" (generar-msg-error "?ERROR DISK FULL" [100 3])))))

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
    (is (= '(5 + 0 / 2 * 0) (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))))

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

(deftest continuar-linea-tests
  (testing "continuar-linea"
    (is (= [nil [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
    (is (= [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])))
    (is (= [:omitir-restante [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [[20 1]] [] [] 0 {}]] (continuar-linea [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2] [20 1]] [] [] 0 {}])))))

;(deftest borrar
  ;(testing "borrar"
    ;(is (= 5 (calcular-rpn '(5) 1)))))
