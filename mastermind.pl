%Encuentra y agrupa todos los valores que cumplan la regla valido 
todos(L):-findall(V,valido(V),L).

% Verifica que una lista de 4 elementos tenga valores del 1 al 9 sin repetir
valido([X,Y,Z,T]):-
    between(1,9,X),
    between(1,9,Y),
    between(1,9,Z),
    between(1,9,T),
    X\=Y,X\=Z,X\=T,
    Y\=Z,Y\=T,
    Z\=T.

% Ya no se utiliza, encuentra y agrupa todos los valores que cumplan la regla combinacion 
combinaciones(L):-findall(V,combinacion(V),L).

% Ya no se utiliza, genera una lista con todos los pares de valores que vayan
% de 0 a 4 sin que su suma supere 4 (tocados y hundidos)
combinacion([H,T]):-
    between(0,4,H),
    between(0,4,T),
    H + T =< 4.

% LLamada a la funcion principal, comienza la creacion del arbol con la lista de jugadas entera y las combinaciones
% posibles de tocados y hundidos
arbolmaster(A) :-
    todos(L),
    creaArbol(L, [[4,0],[2,2],[1,3],[0,4],[3,0],[2,1],[1,2],[0,3],[2,0],[1,1],[0,2],[1,0],[0,1],[0,0]], A).

% Regla que crea el primer nivel del arbol, la raiz 1234 y sus respectivos 14 arboles
creaArbol(_,[],vacio).
creaArbol([],_,vacio).
creaArbol([Jugada|Resto], [C|Cs], rama(Jugada, [Arbol|Arboles])) :-
    creaArboles(Jugada, Resto, [C|Cs], Arbol),
    creaArboles(Jugada, Resto, Cs, Arboles).

% Regla recursiva que filtra las posibilidades en base a la jugada anterior
% y crea las ramas y subramas de todos los arboles
creaArboles(_,[],_,vacio).
creaArboles(_,_,[],vacio).
creaArboles(Anterior, Jugadas,[C|Cs],rama(NuevaJugada,[Arbol,Arboles])):-
  filtraJugadas(C,Anterior,Jugadas,NuevasJugadas),
  obtenNuevaJugada(NuevasJugadas, NuevaJugada),
  creaArboles(NuevaJugada,NuevasJugadas,[C|Cs], Arbol),
  creaArboles(Anterior, Jugadas, Cs , Arboles).

%Regla que devuelve la cabeza de una lista, se utiliza para obtener la siguiente
%jugada, con lo cual si no hay jugadas devuelve vacio
obtenNuevaJugada([],vacio).
obtenNuevaJugada([X|_],X).

%Filtrajugadas recibe el par de tocados y hundidos, la jugada anterior y la lista de
%jugadas a filtrar, devuelve una lista de jugadas nueva
%Cuando se tienen 4 hundidos ya no hay jugadas porque se ha llegado al resultado
filtraJugadas([4,0],_,_,[]).
%Se permuta un valor y se cambian 3
filtraJugadas([0,1],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtener 2 valores distintos que esten en L y N
            member(X1,L),
            member(X2,N),
            %Se permuta un valor
            mismoValorNoPos(X1,X2,L,N),
            %Se obtienen tres valores que esten en las jugadas pero no en la lista actual
            member(Y1,N),
            not(member(Y1,L)),
            member(Z1,N),
            not(member(Z1,L)),
            member(T1,N),
            not(member(T1,L)),
            Y1\=Z1,
            Y1\=T1,
            Z1\=T1
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Se mantiene un valor y se cambian 3
filtraJugadas([1,0],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtener 2 valores distintos que esten en L y N
            member(X1,L),
            member(X2,N),
            %Se mantiene un valor
            mismaPosValor(X1,X2,L,N),
            %Se obtienen tres valores que esten en las jugadas pero no en la lista actual
            member(Y1,N),
            not(member(Y1,L)),
            member(Z1,N),
            not(member(Z1,L)),
            member(T1,N),
            not(member(T1,L)),
            Y1\=Z1,
            Y1\=T1,
            Z1\=T1
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Se permutan dos valores y se cambian dos
filtraJugadas([0,2],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtener 4 valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,_,_]),
            cuatroValoresEn(N,[X2,Y2,_,_]),
            %Se permutan dos valores
            mismoValorNoPos(X1,X2,L,N),
            mismoValorNoPos(Y1,Y2,L,N),
            %Se obtienen dos valores que esten en las jugadas pero no en la lista actual
            member(Z1,N),
            not(member(Z1,L)),
            member(T1,N),
            not(member(T1,L)),
            Z1\=T1
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Se mantienen un valor, se permuta uno y se cambian dos
filtraJugadas([1,1],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtener 4 valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,_,_]),
            cuatroValoresEn(N,[X2,Y2,_,_]),
            %Se mantiene un valor
            mismaPosValor(X1,X2,L,N),
            %Se permuta un valor
            mismoValorNoPos(Y1,Y2,L,N),
            %Se obtienen dos valores que esten en las jugadas pero no en la lista actual
            member(Z1,N),
            not(member(Z1,L)),
            member(T1,N),
            not(member(T1,L)),
            Z1\=T1
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Se mantienen dos valores y se cambian los otros dos
filtraJugadas([2,0],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtener 4 valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,_,_]),
            cuatroValoresEn(N,[X2,Y2,_,_]),
            %Se mantienen dos valores
            mismaPosValor(X1,X2,L,N),
            mismaPosValor(Y1,Y2,L,N),
            %Se obtienen dos valores que esten en las jugadas pero no en la lista actual
            member(Z1,N),
            not(member(Z1,L)),
            member(T1,N),
            not(member(T1,L)),
            Z1\=T1
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Permuta tres valores y cambia el otro
filtraJugadas([0,3],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtener 6 valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,Z1,_]),
            cuatroValoresEn(N,[X2,Y2,Z2,_]),
            %Se permutan tres valores
            mismoValorNoPos(X1,X2,L,N),
            mismoValorNoPos(Y1,Y2,L,N),
            mismoValorNoPos(Z1,Z2,L,N),
            %Se obtiene un valor que este en las jugadas pero no en la lista actual
            member(T1,N),
            not(member(T1,L))
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Mantiene un valor, permuta dos y cambia uno
filtraJugadas([1,2],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtener 6 valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,Z1,_]),
            cuatroValoresEn(N,[X2,Y2,Z2,_]),
            %Se mantiene un valor
            mismaPosValor(X1,X2,L,N),
            %Se permutan dos valores
            mismoValorNoPos(Y1,Y2,L,N),
            mismoValorNoPos(Z1,Z2,L,N),
            %Se obtiene un valor que este en las jugadas pero no en la lista actual
            member(T1,N),
            not(member(T1,L))
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Mantiene dos valores, permuta uno y cambia uno
filtraJugadas([2,1],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtener 6 valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,Z1,_]),
            cuatroValoresEn(N,[X2,Y2,Z2,_]),
            %Se mantienen 2 valores
            mismaPosValor(X1,X2,L,N),
            mismaPosValor(Y1,Y2,L,N),
            %Se permuta un valor
            mismoValorNoPos(Z1,Z2,L,N),
            %Se obtiene un valor que este en las jugadas pero no en la lista actual
            member(T1,N),
            not(member(T1,L))
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Cambiar uno de los valores por otro que no este en la lista y mantener otros tres en la misma pos
filtraJugadas([3,0],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtener 6 valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,Z1,_]),
            cuatroValoresEn(N,[X2,Y2,Z2,_]),
            %Se mantienen 3 valores
            mismaPosValor(X1,X2,L,N),
            mismaPosValor(Y1,Y2,L,N),
            mismaPosValor(Z1,Z2,L,N),
            %Se obtiene un valor que este en las jugadas pero no en la lista actual
            member(T1,N),
            not(member(T1,L))
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Permutar los cuatro valores
filtraJugadas([0,4],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtenemos las listas con los mismos valores
            mismosValores(N,L),
            %Obtener ocho valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,Z1,T1]),
            cuatroValoresEn(N,[X2,Y2,Z2,T2]),
            %Se permutan los cuatro valores
            mismoValorNoPos(X1,X2,L,N),
            mismoValorNoPos(Y1,Y2,L,N),
            mismoValorNoPos(Z1,Z2,L,N),
            mismoValorNoPos(T1,T2,L,N)
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Mantener la posicion de uno y permutar los otros 3
filtraJugadas([1,3],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtenemos las listas con los mismos valores
            mismosValores(N,L),
            %Obtener ocho valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,Z1,T1]),
            cuatroValoresEn(N,[X2,Y2,Z2,T2]),
            %Uno debe de tener la misma posicion y valor
            mismaPosValor(X1,X2,L,N),
            %Los otros tres deben de tener el mismo valor pero distinta posicion
            mismoValorNoPos(Y1,Y2,L,N),
            mismoValorNoPos(Z1,Z2,L,N),
            mismoValorNoPos(T1,T2,L,N)
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Mantener dos y permutar dos
filtraJugadas([2,2],L,Jugadas,Jugadas3):-
    findall(N, 
        (
            member(N,Jugadas),
            %Obtenemos las listas con los mismos valores
            mismosValores(N,L),
            %Obtener ocho valores distintos que esten en L y N
            cuatroValoresEn(L,[X1,Y1,Z1,T1]),
            cuatroValoresEn(N,[X2,Y2,Z2,T2]),
            %Dos deben de tener la misma posicion y valor
            mismaPosValor(X1,X2,L,N),
            mismaPosValor(Y1,Y2,L,N),
            %Los otros dos deben de tener el mismo valor pero distinta posicion
            mismoValorNoPos(Z1,Z2,L,N),
            mismoValorNoPos(T1,T2,L,N)
        ),
        Jugadas2),
    quitarepetidos(Jugadas2,Jugadas3).

%Cambiar los cuatro valores
filtraJugadas([0,0],[X,Y,Z,T],Jugadas,Jugadas2):-
    findall(N,
            (
            member(N,Jugadas),
            not(member(X,N)),
            not(member(Y,N)),
            not(member(Z,N)),
            not(member(T,N))
            ),
        Jugadas2).

%filtraJugadas(_,_,_,[]).

%Devuelve verdadero si dos listas tienen los mismos valores sin importar el orden
mismosValores(L1,L2):-
    sort(L1,L1n),
    sort(L2,L2n),
    L1n = L2n.

%Devuelve verdadero si dos elementos tienen la misma posicion y valor en sus respectivas listas
mismaPosValor(X, Y, L1,L2) :-
    nth0(Pos1, L1, X),
    nth0(Pos2, L2, Y),
    Pos1 = Pos2,
    X = Y.

%Devuelve verdadero si dos elementos tienen el mismo valor pero estrictamente distinta posicion
%en sus respectivas listas
mismoValorNoPos(X, Y, L1, L2) :-
    nth0(Pos1, L1, X),
    nth0(Pos2, L2, Y),
    Pos1 \= Pos2,
    X = Y.

%Devuelve cuatro valores distintos que pertenecen a una lista L
cuatroValoresEn(L, [X,Y,Z,T]):-
    member(X,L),
    member(Y,L),
    member(Z,L),
    member(T,L),
    X\=Y,X\=Z,X\=T,
    Y\=Z,Y\=T,
    Z\=T.

%Elimina los elementos repetidos de una lista
quitarepetidos([],[]).
quitarepetidos([X|Xs],Ys):- member(X,Xs),!,quitarepetidos(Xs,Ys).
quitarepetidos([X|Xs],[X|Ys]):-quitarepetidos(Xs,Ys).

