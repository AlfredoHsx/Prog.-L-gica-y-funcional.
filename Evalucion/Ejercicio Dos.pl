persona(barbara,mujer).
persona(roberto,hombre).
persona(susana,mujer).
persona(juan,hombre).
persona(maria,mujer).
persona(pedro,hombre).

amistad(barbara,roberto).
amistad(susana,juan).
amistad(barbara,juan).
amistad(barbara,maria).
amistad(susana,pedro).

sospechoso(Nombre) :- findall(Nombre,(persona(Nombre,hombre), amistad(susana,Nombre); 
    (persona(Nombre,mujer), amistad(Nombre,Hombre), amistad(susana,Hombre), Nombre \= susana) ;
    (amistad(susana, Hombre), amistad(Mujer,Hombre),persona(Mujer,mujer), amistad(Mujer,Nombre), Nombre \=susana)),Lista),
    list_to_set(Lista, ListaSinDuplicados),member(Nombre,ListaSinDuplicados).