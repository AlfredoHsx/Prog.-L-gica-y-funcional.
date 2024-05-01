amistad(juan,anacleta).
amistad(anastacia,jose).
amistad(roberto,jeremias).
amistad(ana,jose).
amistad(analia,juan).
vive_cerca(aida).
vive_cerca(analia).
conductor_seguro(juan).
conductor_seguro(jose).
conductor_seguro(jeremias).
conductor_seguro(jorge).
bebio(jose,jugo).
bebio(jeremias,jugo).
bebio(juan,vino).
bailo(jorge).
no_trajo_auto(jorge).
vive_lejos(roberto).
vive_lejos(raul).
vive_lejos(rodrigo).
vive_lejos(ana).
vive_lejos(anacleta).
vive_lejos(anastacia).

llega_seguro_sin_ayuda(Nombre) :- vive_cerca(Nombre) ; bebio(Nombre,jugo); bailo(Nombre).

llega_seguro_con_ayuda(Nombre) :- vive_lejos(Nombre), (bebio(Nombre,jugo) ; 
    (amistad(Nombre,Amigo), conductor_seguro(Amigo), \+ bebio(Amigo,vino), \+ no_trajo_auto(Amigo))).

llega_seguro(Nombre) :- llega_seguro_sin_ayuda(Nombre) ; llega_seguro_con_ayuda(Nombre).

no_llega_seguro(Nombre) :- \+ llega_seguro(Nombre).