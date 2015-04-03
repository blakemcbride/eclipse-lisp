(let ()
(defpackage :eclipse-c
  (:nicknames :ec #+machine-compile :c)
  (:use)
  (:export
   function macro
   * + - / % == < > <= >=
   void char wchar_t int wint_t unsigned float double va_list
   long short return
   exp pow log sqrt hypot
   sin cos tan asin acos atan atan2
   sinh cosh tanh asinh acosh atanh
   aint ceil floor ldexp 
   isprint isalpha isupper islower isalnum toupper tolower
   iswprint iswalpha iswupper iswlower iswalnum towupper towlower
   sleep exit
   ))
)  
