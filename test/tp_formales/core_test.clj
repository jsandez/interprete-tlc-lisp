(ns tp-formales.core-test
  (:require [clojure.test :refer :all]
            [tp-formales.core :refer :all]))

(deftest controlar-aridad?-test
  (testing "Prueba de la funcion: controlar-aridad?"
    (is (= 3 (controlar-aridad '(a b c) 3)))
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c) 2)))
    (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))))

(deftest igual?-test
  (testing "Prueba de la funcion: igual?"
    (is (= (igual? 1 1) true))
    (is (= (igual? 1 2) false))
    (is (= (igual? 'a 'a) true))
    (is (= (igual? 'aa123 'AA123) true))
    (is (= (igual? 'A 'A) true))
    (is (= (igual? 'a 'A) true))
    (is (= (igual? 'A 'a) true))
    (is (= (igual? 'a 'b) false))
    (is (= (igual? '(a b c) '(A B C)) true))
    (is (= (igual? '(a b c) '(A B D)) false))
    (is (= (igual? nil nil) true))
    (is (= (igual? nil 'NIL) true))
    (is (= (igual? 'NIL nil) true))
    (is (= (igual? 'NIL 'NIL) true))
    (is (= (igual? nil ()) true))
    (is (= (igual? 'NIL ()) true))
    (is (= (igual? () ()) true))
    (is (= (igual? () '(nil)) false))
    (is (= (igual? "a" "a") true))
    (is (= (igual? "a" "A") false))
    (is (= (igual? 'a "a") false))
    (is (= (igual? 'a "A") false))))

(deftest error?-test
  (testing "Prueba de la funcion: error?"
    (is (= (error? '(*error* too-few-args)) true))
    (is (= (error? (list '*error* 'too-few-args)) true))
    (is (= (error? (list '*ERROR* 'too-few-args)) true))
    (is (= (error? (list '*Error* 'too-few-args)) true))
    (is (= (error? (list '*error*)) true))
    (is (= (error? (list 'too-few-args)) false))
    (is (= (error? '*error*) false))
    (is (= (error? ()) false))
    (is (= (error? nil) false))))

(deftest revisar-fnc?-test
  (testing "Prueba de la funcion: revisar-fnc?"
    (is (= (revisar-fnc '(*error* too-few-args)) '(*error* too-few-args)))
    (is (= (revisar-fnc '(too-few-args)) nil))
    (is (= (revisar-fnc '*error*) nil))
    (is (= (revisar-fnc nil) nil))
    (is (= (revisar-fnc ()) nil))))

(deftest revisar-lae-test
  (testing "Prueba de la funcion: revisar-lae?"
    (is (= (revisar-lae '(1 2 3)) nil))
    (is (= (revisar-lae nil) nil))
    (is (= (revisar-lae ()) nil))
    (is (= (revisar-lae '(1 (*error* too-few-args) 3)) '(*error* too-few-args)))
    (is (= (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3)) '(*error* too-few-args)))))

(deftest actualizar-amb-test
  (testing "Prueba de la funcion: actualizar-amb"
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'd 4) '(a 1 b 2 c 3 d 4)))
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b 4) '(a 1 b 4 c 3)))
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho)) '(a 1 b 2 c 3)))
    (is (= (actualizar-amb () 'b 7) '(b 7)))))

(deftest buscar-test
  (testing "Prueba de la funcion: buscar"
    (is (= (buscar 'c '(a 1 b 2 c 3 d 4 e 5)) 3))
    (is (= (buscar 'f '(a 1 b 2 c 3 d 4 e 5)) '(*error* unbound-symbol f)))))

(deftest fnc-append-test
  (testing "Prueba de la funcion: fnc-append"
    (is (= (fnc-append '((1 2))) '(*error* too-few-args)))
    (is (= (fnc-append '((1 2) (3) (4 5) (6 7))) '(*error* too-many-args)))
    (is (= (fnc-append '((1 2) 3)) '(*error* list expected 3)))
    (is (= (fnc-append '((1 2) A)) '(*error* list expected A)))
    (is (= (fnc-append '((1 2) (3))) '(1 2 3)))
    (is (= (fnc-append '((1 2) nil)) '(1 2)))
    (is (= (fnc-append '(() (1 2))) '(1 2)))
    (is (= (fnc-append '(nil nil)) nil))
    (is (= (fnc-append '(() ())) nil))))

(deftest fnc-env-test
  (testing "Prueba de la funcion: fnc-env"
    (is (= (fnc-env () '(a 1 b 2) '(c 3 d 4)) '(a 1 b 2 c 3 d 4)))
    (is (= (fnc-env '(5) '(a 1 b 2) '(c 3 d 4)) '(*error* too-many-args)))))

(deftest fnc-equal-test
  (testing "Prueba de la funcion: fnc-equal"
    (is (= (fnc-equal '(1 1)) 't))
    (is (= (fnc-equal '(A a)) 't))
    (is (= (fnc-equal '("1" "1")) 't))
    (is (= (fnc-equal '(nil NIL)) 't))
    (is (= (fnc-equal '(1 2)) nil))
    (is (= (fnc-equal '(A B)) nil))
    (is (= (fnc-equal '("1" 1)) nil))
    (is (= (fnc-equal ()) '(*error* too-few-args)))
    (is (= (fnc-equal '(A)) '(*error* too-few-args)))
    (is (= (fnc-equal '(A a A)) '(*error* too-many-args)))))

(deftest fnc-read-test
  (testing "Prueba de la funcion: fnc-read"
    (is (= (with-in-str "1" (fnc-read ())) '1))
    (is (= (with-in-str "a" (fnc-read ())) 'a))
    (is (= (with-in-str "\"hola\"" (fnc-read ())) "hola"))
    (is (= (with-in-str "(hola mundo)" (fnc-read ())) '(hola mundo)))
    (is (= (with-in-str "(hola\nmundo)" (fnc-read ())) '(hola mundo)))
    (is (= (with-in-str "()" (fnc-read ())) nil))
    (is (= (with-in-str "nil" (fnc-read ())) nil))
    (is (= (fnc-read '(1))) '(*error* not-implemented))
    (is (= (fnc-read '(1 2))) '(*error* not-implemented))))

(deftest fnc-terpri-test
  (testing "Prueba de la funcion: fnc-terpri"
    (is (= (with-out-str (fnc-terpri ())) "\n"))
    (is (= (fnc-terpri '(1))) '(*error* not-implemented))
    (is (= (fnc-terpri '(1))) '(*error* not-implemented))))


(deftest fnc-add-test
  (testing "Prueba de la funcion: fnc-add"
    (is (= (fnc-add ()) '(*error* too-few-args)))
    (is (= (fnc-add '(3)) '(*error* too-few-args)))
    (is (= (fnc-add '(3 4)) 7))
    (is (= (fnc-add '(3 4 5)) 12))
    (is (= (fnc-add '(3 4 5 6)) 18))
    (is (= (fnc-add '(A 4 5 6)) '(*error* number-expected A)))
    (is (= (fnc-add '(3 A 5 6)) '(*error* number-expected A)))
    (is (= (fnc-add '(3 4 A 6)) '(*error* number-expected A)))))

(deftest fnc-sub-test
  (testing "Prueba de la funcion: fnc-sub"
    (is (= (fnc-sub ()) '(*error* too-few-args)))
    (is (= (fnc-sub '(3)) -3))
    (is (= (fnc-sub '(3 4)) -1))
    (is (= (fnc-sub '(3 4 5)) -6))
    (is (= (fnc-sub '(3 4 5 6)) -12))
    (is (= (fnc-sub '(A 4 5 6)) '(*error* number-expected A)))
    (is (= (fnc-sub '(3 A 5 6)) '(*error* number-expected A)))
    (is (= (fnc-sub '(3 4 A 6)) '(*error* number-expected A)))))

(deftest fnc-lt-test
  (testing "Prueba de la funcion: fnc-lt"
    (is (= (fnc-lt ()) '(*error* too-few-args)))
    (is (= (fnc-lt '(1)) '(*error* too-few-args)))
    (is (= (fnc-lt '(1 2)) 't))
    (is (= (fnc-lt '(1 1)) nil))
    (is (= (fnc-lt '(2 1)) nil))
    (is (= (fnc-lt '(A 1)) '(*error* number-expected A)))
    (is (= (fnc-lt '(1 A)) '(*error* number-expected A)))
    (is (= (fnc-lt '(1 2 3)) '(*error* too-many-args)))))

(deftest fnc-gt-test
  (testing "Prueba de la funcion: fnc-gt"
    (is (= (fnc-gt ()) '(*error* too-few-args)))
    (is (= (fnc-gt '(1)) '(*error* too-few-args)))
    (is (= (fnc-gt '(2 1)) 't))
    (is (= (fnc-gt '(1 1)) nil))
    (is (= (fnc-gt '(1 2)) nil))
    (is (= (fnc-gt '(A 1)) '(*error* number-expected A)))
    (is (= (fnc-gt '(1 A)) '(*error* number-expected A)))
    (is (= (fnc-gt '(1 2 3)) '(*error* too-many-args)))))

(deftest fnc-ge-test
  (testing "Prueba de la funcion: fnc-ge"
    (is (= (fnc-ge ()) '(*error* too-few-args)))
    (is (= (fnc-ge '(1)) '(*error* too-few-args)))
    (is (= (fnc-ge '(2 1)) 't))
    (is (= (fnc-ge '(1 1)) 't))
    (is (= (fnc-ge '(1 2)) nil))
    (is (= (fnc-ge '(A 1)) '(*error* number-expected A)))
    (is (= (fnc-ge '(1 A)) '(*error* number-expected A)))
    (is (= (fnc-ge '(1 2 3)) '(*error* too-many-args)))))

(deftest fnc-reverse-test
  (testing "Prueba de la funcion: fnc-reverse"
    (is (= (fnc-reverse ()) '(*error* too-few-args)))
    (is (= (fnc-reverse '(1)) '(*error* list expected 1)))
    (is (= (fnc-reverse '(A)) '(*error* list expected A)))
    (is (= (fnc-reverse '((1))) '(1)))
    (is (= (fnc-reverse '((1 2 3))) '(3 2 1)))
    (is (= (fnc-reverse '((1 2 3) (4))) '(*error* too-many-args)))))

(deftest evaluar-escalar-test
  (testing "Prueba de la funcion: evaluar-escalar"
    (is (= (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(32 (v 1 w 3 x 6))))
    (is (= (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("chau" (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(3 (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'x '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(5 (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '((*error* unbound-symbol n) (v 1 w 3 x 6))))))

(deftest evaluar-de-test
  (testing "Prueba de la funcion: evaluar-de"
    (is (= (evaluar-de '(de f (x)) '(x 1)) '(f (x 1 f (lambda (x))))))
    (is (= (evaluar-de '(de f (x) 2) '(x 1)) '(f (x 1 f (lambda (x) 2)))))
    (is (= (evaluar-de '(de f (x) (+ x 1)) '(x 1)) '(f (x 1 f (lambda (x) (+ x 1))))))
    (is (= (evaluar-de '(de f (x y) (+ x y)) '(x 1)) '(f (x 1 f (lambda (x y) (+ x y))))))
    (is (= (evaluar-de '(de f (x y) (prin3 x) (terpri) y) '(x 1)) '(f (x 1 f (lambda (x y) (prin3 x) (terpri) y)))))
    (is (= (evaluar-de '(de) '(x 1)) '((*error* list expected nil) (x 1))))
    (is (= (evaluar-de '(de f) '(x 1)) '((*error* list expected nil) (x 1))))
    (is (= (evaluar-de '(de f 2) '(x 1)) '((*error* list expected 2) (x 1))))
    (is (= (evaluar-de '(de f 2 3) '(x 1)) '((*error* list expected 2) (x 1))))
    (is (= (evaluar-de '(de (f)) '(x 1)) '((*error* list expected nil) (x 1))))
    (is (= (evaluar-de '(de 2 x) '(x 1)) '((*error* list expected x) (x 1))))
    (is (= (evaluar-de '(de 2 (x)) '(x 1)) '((*error* symbol expected 2) (x 1))))
    (is (= (evaluar-de '(de nil (x) 2) '(x 1)) '((*error* cannot-set nil) (x 1))))))

(deftest evaluar-if-test
  (testing "Prueba de la funcion: evaluar-if"
    (is (= (evaluar-if '(if t) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if 7) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if x) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if t 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(9 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if z 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(9 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if w 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(9 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if r 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '((*error* unbound-symbol r) (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil 9) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(nil (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil 9 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil 9 1 2 3 z) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil 9 w) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(3 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil 9 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if nil a 8) '(nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if (gt 2 0) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '((*error* unbound-symbol a) (gt gt nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if (gt 0 2) a 8) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (gt gt nil nil t t v 1 w 3 x 6))))
    (is (= (evaluar-if '(if (gt 0 2) a (setq m 8)) '(gt gt nil nil t t v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(8 (gt gt nil nil t t v 1 w 3 x 6 m 8))))))

(deftest evaluar-or-test
  (testing "Prueba de la funcion: evaluar-or"
    (is (= (evaluar-or '(or) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or t) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(t (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or w) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(5 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or r) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '((*error* unbound-symbol r) (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or y) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil 6) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or (setq b 8) nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(8 (nil nil t t w 5 x 4 b 8))))
    (is (= (evaluar-or '(or nil 6 nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil 6 r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(6 (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil t r nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(t (nil nil t t w 5 x 4))))
    (is (= (evaluar-or '(or nil nil nil nil) '(nil nil t t w 5 x 4) '(x 1 y nil z 3)) '(nil (nil nil t t w 5 x 4))))))

(deftest evaluar-setq-test
  (testing "Prueba de la funcion: evaluar-setq"
    (is (= (evaluar-setq '(setq) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '((*error* list expected nil) (nil nil t t + add w 5 x 4))))
    (is (= (evaluar-setq '(setq m) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '((*error* list expected nil) (nil nil t t + add w 5 x 4))))
    (is (= (evaluar-setq '(setq m 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(7 (nil nil t t + add w 5 x 4 m 7))))
    (is (= (evaluar-setq '(setq x 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(7 (nil nil t t + add w 5 x 7))))
    (is (= (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(2 (nil nil t t + add w 5 x 2))))
    (is (= (evaluar-setq '(setq x (+ x 1)) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(5 (nil nil t t + add w 5 x 5))))
    (is (= (evaluar-setq '(setq nil) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '((*error* list expected nil) (nil nil t t + add w 5 x 4))))
    (is (= (evaluar-setq '(setq nil 7) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '((*error* cannot-set nil) (nil nil t t + add w 5 x 4))))
    (is (= (evaluar-setq '(setq 7 8) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '((*error* symbol expected 7) (nil nil t t + add w 5 x 4))))
    (is (= (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(x 1 y nil z 3)) '(8 (nil nil t t + add w 5 x 7 m 8))))
    (is (= (evaluar-setq '(setq x 7 m (+ x 7)) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(14 (nil nil t t + add w 5 x 7 m 14))))
    (is (= (evaluar-setq '(setq x 7 y) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '((*error* list expected nil) (nil nil t t + add w 5 x 7))))
    (is (= (evaluar-setq '(setq x 7 y 8 z 9) '(nil nil t t + add w 5 x 4) '(y nil z 3)) '(9 (nil nil t t + add w 5 x 7 y 8 z 9))))))