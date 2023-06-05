(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.core :refer :all]))

;;(defn generate-operador?-tests [lst]
;;  (map (fn [kw] `(is (= true (operador? '~kw)))) lst))

(deftest palabra-reservada?-test
  (testing "palabra-reservada?"
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'STR$)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'END)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'LOG)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'IF)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'RETURN)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'ATN)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'PRINT)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'LET)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'READ)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? '?)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'NEW)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'RESTORE)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'LEN)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'CLEAR)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'EXP)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'GOSUB)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'STEP)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'ON)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'GOTO)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'THEN)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'ASC)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'CHR$)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'INPUT)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'TO)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'NEXT)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'REM)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'LIST)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'MID$)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'SIN)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'INT)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'DATA)))
    (clojure.test/is (clojure.core/= true (interprete.core/palabra-reservada? 'FOR)))
    (clojure.test/is (clojure.core/= false (interprete.core/palabra-reservada? 'SPACE)))))

(deftest operador?-tests
  (testing "operador?"
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '+)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '-)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '*)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '/)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '=)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '<>)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '<)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '<=)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '>)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? '>=)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? 'AND)))
    (clojure.test/is (clojure.core/= true (interprete.core/operador? 'OR)))
    (clojure.test/is (clojure.core/= false (interprete.core/operador? 'SPACE)))))

(deftest anular-invalidos-tests
  (testing "anular-invalidos"
    (clojure.test/is (clojure.core/= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))))

(deftest variable-float?-tests
  (testing "variable-float?"
    (clojure.test/is (clojure.core/= true (variable-float? 'X)))
    (clojure.test/is (clojure.core/= false (variable-float? 'X%)))
    (clojure.test/is (clojure.core/= false (variable-float? 'X$)))))

(deftest variable-integer?-tests
  (testing "variable-integer?"
    (clojure.test/is (clojure.core/= true (variable-integer? 'X%)))
    (clojure.test/is (clojure.core/= false (variable-integer? 'X)))
    (clojure.test/is (clojure.core/= false (variable-integer? 'X$)))))

(deftest variable-string?-tests
  (testing "variable-string?"
    (clojure.test/is (clojure.core/= true (variable-string? 'X$)))
    (clojure.test/is (clojure.core/= false (variable-string? 'X)))
    (clojure.test/is (clojure.core/= false (variable-string? 'X%)))))

(deftest precedencia-tests
  (testing "precedencia"
    (clojure.test/is (clojure.core/= 1 (precedencia 'OR)))
    (clojure.test/is (clojure.core/= 2 (precedencia 'AND)))
    (clojure.test/is (clojure.core/= 3 (precedencia 'NOT)))
    (clojure.test/is (clojure.core/= 4 (precedencia '<>)))
    (clojure.test/is (clojure.core/= 4 (precedencia '<)))
    (clojure.test/is (clojure.core/= 4 (precedencia '<=)))
    (clojure.test/is (clojure.core/= 4 (precedencia '>)))
    (clojure.test/is (clojure.core/= 4 (precedencia '>=)))
    (clojure.test/is (clojure.core/= 4 (precedencia '=)))
    (clojure.test/is (clojure.core/= 5 (precedencia '+)))
    (clojure.test/is (clojure.core/= 5 (precedencia '-)))
    (clojure.test/is (clojure.core/= 6 (precedencia '*)))
    (clojure.test/is (clojure.core/= 6 (precedencia '/)))
    (clojure.test/is (clojure.core/= 7 (precedencia '-u)))
    (clojure.test/is (clojure.core/= 8 (precedencia 'MID$)))))

(deftest eliminar-cero-decimal-tests
  (testing "eliminar-cero-decimal"
    (clojure.test/is (clojure.core/= 1.5 (eliminar-cero-decimal 1.5)))
    (clojure.test/is (clojure.core/= 1.5 (eliminar-cero-decimal 1.50)))
    (clojure.test/is (clojure.core/= 1 (eliminar-cero-decimal 1.0)))
    (clojure.test/is (clojure.core/= 'A (eliminar-cero-decimal 'A)))))
