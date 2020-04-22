
hongo(amanita_caesarea):- sombrero_ancho(6,17),
    (sombrero_perfil(plano_convexo);sombrero_perfil(hemisferico)),
    margen(estraido),
    (cuticula_color(amarillo_oro);cuticula_color(amarillo_anaranjado)),
    laminas_forma(libres),
    laminas_color(amarillo),
    pie_largo(7,15),
    anillo(membranoso),
    anillo(estriado),
    anillo_color(amarillo),
    volva_color(blanca),
    volva(amplia),
    volva(membranosa),
    volva(libre),
    volva(forma_saco),
    carne(gruesa),
    carne_color(blanca),
    olor(poco_apeciable),
    (estacion(primavera);estacion(verano);estacion(otoño)).

hongo(amanita_muscaria):-(sombrero_perfil(globos);sombrero_perfil(plano_convexo)),
    cuticula_color(amarillo_oro),
    cuticula(lisa),
    cuticula(con_verrugas_algodonosas_blancas_o_amarillentas),
    laminas_forma(libres),
    laminas_color(blanca),
    pie_largo(17,23),
    anillo(amplio),
    anillo(estriado),
    anillo_color(blanco),
    volva(disociada),
    carne_color(blanca),
    olor(nulo),
    (estacion(otoño);estacion(invierno)).
	
hongo(amanita_verna) :- sombrero_ancho(5,10),
    (sombrero_perfil(plano_convexo);sombrero_perfil(hemisferico)),
	cuticula_color(blanco_marfil),
	cuticula(sin_restos_de_velo),
	laminas_forma(libres),
	laminas_forma(prietas),
	laminas_color(blanca),
	pie_largo(6,10),
	anillo(persistente),
    anillo_color(blanco),
	volva(persistente),
	volva(forma_saco),
	volva_color(blanca),
	carne(tierna),
	carne_color(blanca),
    olor(nulo),
    (estacion(primavera);estacion(verano)).

hongo(cortinarius_orellanus) :- sombrero_ancho(3,8),
    sombrero_perfil(abierto),
	sombrero_perfil(convexo),
	margen(enrollado),
	margen(abierto),
	(cuticula_color(mate);cuticula_color(rojo_naranja_oscuro);cuticula_color(pardo_rojizo)),
	(cuticula(sedosa);cuticula(fibrillosa)),
	laminas_forma(adherentes),
	laminas_forma(espaciadas),
	laminas_forma(gruesas),
	laminas_forma(desiguales),
	(laminas_color(amarillo);laminas_color(rojo_herrumbroso)),
	pie_largo(4,8),
	carne(con_roña_bajo_la_cuticula),
	(carne_color(amarilla);carne_color(leonado)),
    olor(poco_apeciable),
	olor(a_rabano),
    (estacion(otoño);estacion(verano)).

hongo(amanita_phalloides) :- sombrero_ancho(5,15),
    (sombrero_perfil(plano_convexo);sombrero_perfil(ovoide)),
	(cuticula_color(oliva);cuticula_color(verde_amarillento);cuticula_color(casi_blanco)),
	cuticula(brillante),
	cuticula(glabra),
	laminas_forma(libres),
	laminas_forma(prietas),
	laminas_color(blanca),
	pie_largo(7,15),
	anillo(amplio),
	anillo(estriado),
    anillo_color(blanco_con_tintes_citrinos),
	volva(membranosa),
	volva(libre),
	volva(desgarrada_en_lobulos),
	volva_color(blanca),
	carne_color(blanca),
    olor(poco_apeciable),
    (estacion(otoño);estacion(verano)).

hongo(boletus_satanas) :- sombrero_ancho(8,30),
    (sombrero_perfil(plano_convexo);sombrero_perfil(hemisferico)),
	(cuticula_color(blanco);cuticula_color(blanco_sucio)),
	cuticula_color(sin_coloracion_rosa),
	pie_largo(5,15),
	carne(compacta),
	carne_color(blanca),
	carne_color(azul_al_corte),
    olor(desagradable),
    (estacion(otoño);estacion(verano);estacion(primavera)).

hongo(gyoromitra_esculenta) :- sombrero_ancho(2,5),
    sombrero_perfil(globos),
	sombrero_perfil(con_pliegues),
	(cuticula_color(pardo_castaño);cuticula_color(pardo_rojizo);cuticula_color(casi_negro)),
	margen(enrollado),
	pie_largo(2,5),
	carne(escasa),
	carne(fragil),
	carne_color(blanco_grisacea),
    olor(espermatico),
    estacion(primavera).


sombrero_ancho(X,Y):- medir(sombrero_ancho, X, Y).
sombrero_perfil(X):- menuask(sombrero_perfil, X, [plano_convexo,hemisferico,globos,abierto,convexo,ovoide,con_pliegues]).
margen(X):- menuask(margen, X, [estraido,enrollado,abierto]).
cuticula(X):-menuask(cuticula, X, [lisa,con_verrugas_algodonosas_blancas_o_amarillentas,sin_restos_de_velo,sedosa,fibrillosa,brillante,glabra]).
cuticula_color(X):- menuask(cuticula_color, X, [amarillo_oro,amarillo-anaranjado,blanco_marfil,mate,rojo_naranja_oscuro,pardo_rojizo,oliva,verde_amarillento,casi_blanco,blanco,blanco_sucio,sin_coloracion_rosa,pardo_castaño,casi_negro]).
laminas_forma(X):- menuask(laminas_forma, X, [libres,prietas,adherentes,espaciadas,gruesas,desiguales]).
laminas_color(X):- menuask(laminas_color, X, [amarillo,blanca,rojo_herrumbroso]).
pie_largo(X,Y):- medir(pie_largo, X, Y).
anillo(X):- menuask(anillo, X, [membranoso,amplio,estriado,persistente]).
anillo_color(X):- menuask(anillo_color, X, [amarillo,blanco,blanco_con_tintes_citrinos]).
volva(X):- menuask(volva, X, [amplia,membranosa,libre,forma_saco,disociada,persistente,desgarrada_en_lobulos]).
volva_color(X):- menuask(volva_color, X, [blanca]).
carne(X):- menuask(carne, X, [gruesa,tierna,con_roña_bajo_la_cuticula,compacta,escasa,fragil]).
carne_color(X):- menuask(carne_color, X, [blanca,blanco_grisacea,azul_al_corte,amarilla,leonada]).
olor(X):- menuask(olor, X, [poco_apeciable, nulo,a_rabano,desagradable,espermatico]).
estacion(X):- menuask(estacion, X, [primavera,verano,otoño,invierno]).


menuask(A, V, MenuList) :- 
	write('¿Cual es el valor para: '), 
	write(A), 
	write('?'), 
	nl,
	write(MenuList),
	nl,
	read(X),
	check_val(X, A, V, MenuList),
	asserta( known(yes, A, X) ),
	X == V.
check_val(X, A, V, MenuList) :- 
	member(X, MenuList), 
	!.
check_val(X, A, V, MenuList) :- 
	write(X),
	write(' no es valor valido, pruebe de nuevo.'),
	nl,
	menuask(A, V, MenuList).


ask(A, V):-
    known(yes, A, V),  % succeed if true
    !.  % stop looking
ask(A, V):-
    known(_, A, V),  % fail if false
    !,   fail.
ask(A, V):-
    write(A:V),  % ask user
    write('? : '),
    read(Y),  % get the answer
    asserta(known(Y, A, V)),  % remember it
    Y == yes.  % succeed or fail


medir(A,X,Y):-
    known(N,A,_),
    (N>=X,N=<Y),
    !.
medir(A,X,Y):-
    known(N,A,_),
    (N<X;N>Y),
    !, fail.
medir(A,X,Y):-
    write('¿Cuántos centímetros mide '),
    write(A),
    write(?),
    read(N),
    asserta(known(N,A,_)),
    (N>=X,N=<Y).


top_goal(X):-hongo(X).


solve :-
    dynamic(known/3),
    retractall(known(_,_,_)),
    top_goal(X),
    write('The answer is '),
    write(X), nl.
solve :-
    write('No answer found.'),
    nl.
