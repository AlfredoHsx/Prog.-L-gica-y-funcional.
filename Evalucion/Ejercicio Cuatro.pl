% Hechos de conexiones entre ciudades y sus distancias
conexion(nueva_cordoba, alta_cordoba, 3000).
conexion(nueva_cordoba, casco_centrico, 200).
conexion(casco_centrico, villa_allende, 4000).
conexion(alta_cordoba, villa_allende, 750).
conexion(nueva_cordoba, villa_arguello, 5000).
conexion(villa_arguello, villa_allende, 3000).
conexion(nueva_cordoba, las_palmas, 4000).
conexion(las_palmas, villa_allende, 3200).
conexion(nueva_cordoba, los_platanos, 2000).
conexion(los_platanos, san_fernando, 3000).
conexion(san_fernando, villa_allende, 1700).

% Encuentra todas las rutas posibles
ruta(Inicio, Fin, Camino, Distancia) :-
    ruta_aux(Inicio, Fin, [Inicio], Camino, 0, Distancia).

ruta_aux(Fin, Fin, Camino, Camino, Distancia, Distancia).
ruta_aux(Inicio, Fin, Visitados, Camino, DistAcc, Distancia) :-
    conexion(Inicio, Siguiente, Dist),
    \+ member(Siguiente, Visitados),
    DistNuevo is DistAcc + Dist,
    ruta_aux(Siguiente, Fin, [Siguiente|Visitados], Camino, DistNuevo, Distancia).

% Ruta más corta
ruta_mas_corta(Inicio, Fin, Camino, Distancia) :-
    setof([Cam, Dist], ruta(Inicio, Fin, Cam, Dist), Rutas),
    Rutas = [_|_], 
    minimal(Camino, Distancia, Rutas).

% Ruta más larga
ruta_mas_larga(Inicio, Fin, Camino, Distancia) :-
    setof([Cam, Dist], ruta(Inicio, Fin, Cam, Dist), Rutas),
    Rutas = [_|_], 
    maximal(Camino, Distancia, Rutas).

% Encuentra la ruta con la menor distancia
minimal(Camino, Distancia, [[Camino, Distancia]|_]).

% Encuentra la ruta con la mayor distancia
maximal(Camino, Distancia, Rutas) :-
    reverse(Rutas, [[Camino, Distancia]|_]).
