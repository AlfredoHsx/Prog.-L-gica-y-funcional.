padre(marcelo,raul).
padre(marcelo,rita).
padre(juan,marcelo).
padre(juan,maria).
padre(raul,sergio).

madre(mirian, ricardo).
madre(rita,victor).
madre(rita,veronica).

linda(mirian).
linda(rita).

abuelo(Abuelo,Nieto) :- padre(Abuelo,Padre), padre(Padre,Nieto).
abuelo(Abuelo,Nieto) :- padre(Abuelo,Madre), madre(Madre,Nieto).

nieto(Nieto,Abuelo) :- abuelo(Abuelo,Nieto).

hija(Hija,Padre) :- padre(Padre,Hija).
hija(Hija,Madre) :- madre(Madre,Hija).

hermana(Hermana1, Hermana2) :- madre(Madre,Hermana1), madre(Madre,Hermana2).
hermana(Hermana1, Hermana2) :- padre(Padre, Hermana1), padre(Padre,Hermana2).

casado(Hombre,Mujer) :- madre(Mujer,Hija), padre(Hombre,Hija).

casado_con_alguien_lindo(Hombre,Mujer) :- casado(Hombre,Mujer), linda(Mujer).

relacion_familiar(Persona1, Persona2) :- (madre(Persona1,Persona2), linda(Persona1), linda(Persona2)); 
    (madre(Persona2,Persona1), linda(Persona1), linda(Persona2)); 
    (padre(Persona1,Persona2), linda(Persona1), linda(Persona2)); (padre(Persona2,Persona1), linda(Persona1), linda(Persona2));
    (abuelo(Persona1,Persona2), linda(Persona1), linda(Persona2)); (abuelo(Persona2,Persona1), linda(Persona1), linda(Persona2)).

primo_de(Primo, Persona) :-
    padre(Padre1, Persona),
    padre(Padre2, Primo),
    hermana(Padre1, Padre2),
    Padre1 \= Padre2.

primo_de(Primo, Persona) :-
    madre(Madre1, Persona),
    madre(Madre2, Primo),
    hermana(Madre1, Madre2),
    Madre1 \= Madre2.

primo_de(Primo, Persona) :-
    padre(Padre1, Persona),
    madre(Madre2, Primo),
    hermana(Madre2, Padre1).

primo_de(Primo, Persona) :-
    madre(Madre1, Persona),
    padre(Padre2, Primo),
    hermana(Madre1, Padre2).

tio(Tio,Sobrino) :- primo_de(Primo,Sobrino), (madre(Tio,Primo); padre(Tio,Primo)).
