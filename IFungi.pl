:- discontiguous multivalued/1.
:- discontiguous link/2.
:- discontiguous hongo/1.

hongo(amanita_caesarea):-
    tipo(sombrero_laminas),
    (sombrero_perfil(plano_convexo);sombrero_perfil(hemisferico)),
    sombrero_ancho(6,17),
    margen(estraido),
    cuticula_color(amarillo),
    laminas_forma(libres),
    laminas_color(amarillo),
    pie_largo(7,15),
    anillo(estriado),
    anillo_color(amarillo),
    volva_color(blanca),
    volva(membranosa),
    volva(forma_saco),
    carne(gruesa),
    carne_color(blanco),
    (estacion(primavera);estacion(verano);estacion(otoño)).
link(amanita_caesarea, 'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos/#amanita-caesarea').

hongo(agaricus_campestris):-
    tipo(sombrero_laminas),
    (sombrero_perfil(convexo);sombrero_perfil(hemisferico);sombrero_perfil(aplanado)),
    sombrero_ancho(4,11),
    margen(excedente),
    cuticula_color(blanco),
    laminas_forma(libres),
    (laminas_color(rosa);laminas_color(negro);laminas_color(marron)),
    pie_largo(3,6),
    anillo_color(blanco),
    anillo(membranoso),
    carne_color(blanco),
    (estacion(primavera);estacion(verano);estacion(otoño);estacion(invierno)).
link(agaricus_campestris,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos/#agaricus-campestris').

hongo(boletus_edulis):-
    tipo(sombrero_poros),
    (sombrero_perfil(convexo);sombrero_perfil(hemisferico)),
    sombrero_ancho(5,20),
    cuticula_color(pardo),
    pie_largo(4,20),
    pie(robusto),
    poros(redondos),
    (poros_color(blanco); poros_color(amarillo); poros_color(verde)),
    tubos(largos),
    carne_color(blanco),
    (estacion(primavera);estacion(verano)).
link(agaricus_campestris,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos/#boletus-edulis').

hongo(lactarius_delicious):-
    tipo(sombrero_laminas),
    (sombrero_perfil(convexo);sombrero_perfil(deprimido)),
    sombrero_ancho(5,13),
    cuticula(viscosa),
    (cuticula_color(naranja);cuticula_color(rojo)),
    laminas_forma(adnatas),
    laminas_color(naranja),
    pie_largo(3,7),
    carne(gruesa),
    carne_color(rojo),
    estacion(otoño).
link(lactarius_delicious,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos/#lactarius-deliciosus').

hongo(cantharellus_cibarius):-
    tipo(himenio),
    (sombrero_perfil(convexo);sombrero_perfil(extendido);sombrero_perfil(embudo)),
    sombrero_ancho(3,10),
    cuticula(lisa),
    cuticula_color(amarillo),
    pie_largo(2,8),
    himenio(pliegues_bifurcados),
    carne_color(blanco),
    carne(firme),
    carne(elastica),
    (estacion(primavera);estacion(verano);estacion(otoño)).
link(cantharellus_cibarius,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos/#cantharellus-cibarius').

hongo(tuber_melanosporum):-
    tipo(carpoforo),
    carpoforo(globoso_irregular),
    carpoforo_diametro(2,8),
    peridio(liso),
    (peridio_color(negro);peridio_color(ocre)),
    gleba(compacta),
    gleba(venas_blancas),
    gleba_color(negro),
    estacion(invierno).
link(tuber_melanosporum,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos/#tuber-melanosporum').


hongo(amanita_muscaria):-
     tipo(sombrero_laminas),
    (sombrero_perfil(globos);sombrero_perfil(plano_convexo)),
    cuticula_color(rojo),
    cuticula(lisa),
    cuticula(con_verrugas_algodonosas_blancas_o_amarillentas),
    laminas_color(blanco),
    pie_largo(17,23),
    anillo(estriado),
    anillo_color(blanco),
    volva(disociada),
    carne_color(blanco),
    (estacion(otoño);estacion(invierno)).
link(amanita_muscaria,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos/#amanita-muscaria').


hongo(amanita_verna) :-
    tipo(sombrero_laminas),
    (sombrero_perfil(plano_convexo);sombrero_perfil(hemisferico)),
    sombrero_ancho(5,10),
    cuticula_color(blanco),
    cuticula(sin_restos_de_velo),
    laminas_forma(prietas),
    laminas_color(blanco),
    pie_largo(6,10),
    anillo(persistente),
    anillo_color(blanco),
    volva(persistente),
    volva(forma_saco),
    volva_color(blanca),
    carne(tierna),
    carne_color(blanco),
    (estacion(primavera);estacion(verano)).
link(amanita_verna,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos#amanita-verna').


hongo(cortinarius_orellanus) :-
    tipo(sombrero_laminas),
    (sombrero_perfil(abierto);sombrero_perfil(convexo)),
    sombrero_ancho(3,8),
    margen(enrollado),
    margen(abierto),
    (cuticula_color(blanco);cuticula_color(naranja);cuticula_color(rojo)),
    (cuticula(sedosa);cuticula(fibrillosa)),
    laminas_forma(gruesas),
    laminas_forma(desiguales),
    (laminas_color(amarillo);laminas_color(rojo)),
    pie_largo(4,8),
    carne(con_roña_bajo_la_cuticula),
    (carne_color(amarillo);carne_color(naranja)),
    (estacion(otoño);estacion(verano)).
link(cortinarius_orellanus,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos#cortinarus-orellanus').

hongo(amanita_phalloides) :-
    tipo(sombrero_laminas),
    (sombrero_perfil(plano_convexo);sombrero_perfil(ovoide)),
    sombrero_ancho(5,15),
    (cuticula_color(amarillo);cuticula_color(blanco);cuticula_color(verde)),
    cuticula(brillante),
    cuticula(glabra),
    laminas_forma(prietas),
    laminas_color(blanco),
    pie_largo(7,15),
    anillo(amplio),
    anillo(estriado),
    anillo_color(blanco),
    volva(membranosa),
    volva(desgarrada_en_lobulos),
    volva_color(blanca),
    carne_color(blanco),
    (estacion(otoño);estacion(verano)).
link(amanita_phalloides,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos#amanita-phalloides').

hongo(boletus_satanas) :-
    tipo(sombrero_poros),
    (sombrero_perfil(plano_convexo);sombrero_perfil(hemisferico)),
    sombrero_ancho(8,30),
    cuticula_color(blanco),
    pie_largo(5,15),
    poros(pequeños),
    poros_color(rojo),
    tubos(largos),
    carne(compacta),
    carne_color(blanco),
    carne_color(azul_al_corte),
    (estacion(otoño);estacion(verano);estacion(primavera)).
link(boletus_satanas,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos#boletus-satanas').

hongo(gyoromitra_esculenta) :-
    tipo(sombrero),
    sombrero_perfil(globos),
    sombrero_ancho(2,5),
    sombrero_perfil(con_pliegues),
    (cuticula_color(pardo);cuticula_color(rojo);cuticula_color(negro)),
    margen(enrollado),
    pie_largo(2,5),
    carne(escasa),
    carne(fragil),
    carne_color(blanco),
    estacion(primavera).
link(gyoromitra_esculenta,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Setas-y-Hongos#gyromitra-esculenta').

tipo(X):-menuask(tipo, X, [sombrero,sombrero_laminas,sombrero_poros,himenio,carpoforo],nil).

sombrero_ancho(X,Y):- medir(sombrero_ancho, X, Y).

sombrero_perfil(X):- menuask(sombrero_perfil, X, [plano_convexo,hemisferico,globoso,abierto,aplanado,convexo,ovoide,con_pliegues,deprimido,embudo,extendido],'https://github.com/manuvillalba-uclm/Ifungi/wiki/Caracter%C3%ADsticas#sombrero').

margen(X):- ask(margen, X, 'https://github.com/manuvillalba-uclm/Ifungi/wiki/Caracter%C3%ADsticas#margen').
multivalued(margen).

cuticula(X):-  ask(cuticula, X,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Caracter%C3%ADsticas#cuticula').
multivalued(cuticula).

cuticula_color(X):- menuask(cuticula_color, X, [amarillo,rojo,blanco,naranja,pardo,negro,verde],nil).

laminas_forma(X):- ask(laminas_forma, X,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Caracter%C3%ADsticas#himenio').
multivalued(laminas_forma).

laminas_color(X):- menuask(laminas_color, X, [blanco,rosa,amarillo,naranja,blanco,negro,marron],nil).

poros(X):- ask(poros, X,nil).
multivalued(poros).

poros_color(X):- menuask(poros_color, X, [amarillo,blanco,verde,rojo],nil).

tubos(X):- ask(tubos, X,nil).
multivalued(tubos).

pie_largo(X,Y):- medir(pie_largo, X, Y).

pie(X):- ask(pie,X,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Caracter%C3%ADsticas#pie').
multivalued(pie).

anillo(X):- ask(anillo, X,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Caracter%C3%ADsticas#anillo').
multivalued(anillo).

anillo_color(X):- menuask(anillo_color, X, [amarillo,blanco],nil).

volva(X):- ask(volva, X,'https://github.com/manuvillalba-uclm/Ifungi/wiki/Caracter%C3%ADsticas#volva').
multivalued(volva).

volva_color(X):- menuask(volva_color, X, [blanca],nil).

himenio(X):- ask(himenio, X, 'https://github.com/manuvillalba-uclm/Ifungi/wiki/Caracter%C3%ADsticas#himenio').
multivalued(himenio).

carpoforo(X):-ask(carpoforo, X,nil).
multivalued(carpoforo).

carpoforo_diametro(X,Y):- medir(carpoforo_diametro, X, Y).

peridio(X):-ask(peridio, X,nil).
multivalued(peridio).

peridio_color(X):- menuask(peridio_color, X, [negro,ocre],nil).

gleba(X):-ask(gleba, X,nil).
multivalued(gleba).

gleba_color(X):- menuask(peridio_color, X, [negro],nil).

carne(X):- ask(carne, X, 'https://github.com/manuvillalba-uclm/Ifungi/wiki/Caracter%C3%ADsticas#carne').
multivalued(carne).

carne_color(X):- menuask(carne_color, X, [blanco,azul_al_corte,amarillo,naranja,rojo],nil).

estacion(X):- menuask(estacion, X, [primavera,verano,otoño,invierno],nil).


menuask(A, V,_,_):-
    known(yes, A, V),  % succeed if true
    !.  % stop looking
menuask(A, _,_,_):-
    known(yes, A, _),  % fail if false
    !,   fail.

menuask(A, V, MenuList,L) :-
    write('¿Cual es el valor para: '),
    write(A),
    write('?'),
    nl,
    write(MenuList),
    nl,
    (L\==nil -> www_open_url(L);true),
    read(X),
    check_val(X, A, V, MenuList,R),
    asserta( known(yes, A, R) ),
        R==V.


check_val(X, _, _, MenuList,X) :-
    member(X, MenuList),
    !.
check_val(X, A, V, MenuList,R) :-
    write(X),
    write(' no es valor valido, pruebe de nuevo.'),
    nl,
        write(MenuList),
        nl,
        read(Y),
    check_val(Y,A, V, MenuList,R).


ask(A, V,_):-
    known(yes, A, V),  % succeed if true
    !.  % stop looking
ask(A, V,_):-
    known(_, A, V),  % fail if false
    !,   fail.
ask(A, V,L):-
    write(A:V),  % ask user
    write('? : '),
    (L\==nil -> www_open_url(L)),
    read(Y),  % get the answer
    asserta(known(Y, A, V)),  % remember it
    Y == yes.  % succeed or fail
ask(A, V,_):-
    not(multivalued(A)),
    known(yes, A, V2),
    V \== V2,
    !,
    fail.

medir(A,X,Y):-
    known(_,A,N),
    (N>=X,N=<Y),
    !.
medir(A,X,Y):-
    known(_,A,N),
    (N<X;N>Y),
    !, fail.
medir(A,X,Y):-
    write('¿Cuántos centímetros mide '),
    write(A),
    write(?),
    read(N),
    asserta(known(yes,A,N)),
    (N>=X,N=<Y).


top_goal(X):- hongo(X).

solve:-
    dynamic(known/3),
    retractall(known(_,_,_)),
    bagof(X,top_goal(X),[X2|XLista]),
    nl,nl,nl,
    write('Tu descripción encaja con: '),
    write(X2), nl,
    link(X2,Y),
    listing(known(_,_,_)),
    www_open_url(Y),
    imprimir(XLista).
solve :-
    write('No answer found.'),
    nl,
    listing(known(_,_,_)).

imprimir([]).
imprimir([X]):-
    write('y con: '),
    write(X), nl,
    link(X,Y),
    www_open_url(Y).

imprimir([X|XLista]):-
    write(', con: '),
    write(X), nl,
    link(X,Y),
    www_open_url(Y),
    imprimir(XLista).

