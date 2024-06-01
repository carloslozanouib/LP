% AUTORES: MICHAEL JAVIER CATAÑO ORTIZ Y CARLOS LOZANO ALEMAÑY
% Implementaciones hechas: ciutat quadrada,rectangular e inteficie de usuario

% Predicado ciutats/5 principal que resuelve el problema de las ciudades.
% CF1, CF2, CC1, CC2 son las pistas dadas, donde
% CFi son las listas que condiconan las filas y 
% CCi son las listas que condiconan las columnas.
% L es la solución del problema.
ciutats(CF1,CF2,CC1,CC2,L):-
    length(CC1,N), % calculo N atravez de la lista CC1
    rellenar(1,N,L1), % creo una lista que contiene numeros que van desde 1 hasta N
   	ciutats(CF1,CF2,L1,L), % predicado que calcula las permutaciones de la lista L1 que cumplan las condicones
    traspuesta(L,L2),      % predicado que crea la traspuesta de una lista de listas.
    comprobacion_columnas(CC1,CC2,L2), !, % se comprueba que la traspuesta  sea correcta
    imprimir_taulell(CF1,CF2,CC1,CC2,L).  % imprimo la solucion

% Predicado que imprime el tablero de solución.
% CF1, CF2, CC1, CC2 son las pistas.
% L es la solución del problema.
imprimir_taulell(_,_,_,[],[]).
imprimir_taulell(CF1,CF2,CC1,CC2,L):-
    write('***'),
    imprimir_fila(CC1),
    write('***'),
    nl,
    imprimir_elem(CF1,CF2,L),
    write('***'),
    imprimir_fila(CC2),
    write('***').

% Predicado aux que imprime los elementos de la solución de manera correcta.
imprimir_elem([],[],[]).
imprimir_elem([X|CF1],[Y|CF2],[Z|L]):-
    write(X),
    write('|'),
    write(' '),
    imprimir_fila(Z),
    write(' '),
    write('|'),
    write(Y),
    nl,
    imprimir_elem(CF1,CF2,L).

% Predicado que imprime una fila.
imprimir_fila([]).
imprimir_fila([Elemento|Resto]) :-
    write(Elemento),
    write(' '),
    imprimir_fila(Resto).
    
% Predicado que comprueba si las columnas cumplen con las pistas CCi.
comprobacion_columnas([],[],[]).
comprobacion_columnas([X|CC1],[Y|CC2],[Z|L2]):-
    ciutats(X,Y,Z),
    comprobacion_columnas(CC1,CC2,L2).

% Predicado ciutats/4 que resuelve el problema por filas.
% este predicado lo que hace es apartir de la lista p, crea permutaciones de ella
% y comprueba con ciutats/3 que esa permutacion cumple las condiciones por fila
% cada permutaicon que lo cumpla se añade a la lista de soluciones.
ciutats(_,_,_,[]).
ciutats([X|CF1],[Y|CF2],P,[Perms|L]):-
    permuta(P,Perms),
    ciutats(X,Y,Perms),
    ciutats(CF1,CF2,P,L).

% Predicado ciutats/3 que comprueba si una lista cumple con las condiciones
% C1 Y C2
ciutats(C1,C2,L):-
    numeros_vistos(L,P),
    C1 = P,
    invertir(L,L2),
    numeros_vistos(L2,P2),
    C2 = P2, !.
     
% Predicado que cuenta los números visibles en una lista.
numeros_vistos([], 0).
numeros_vistos([X|L], N) :- numeros_vistos(L, X, 1, N).

% Predicado auxiliar que cuenta los números visibles en una lista.
numeros_vistos([], _, N, N).
numeros_vistos([X|L], M, C, N) :-
    X > M,
    C1 is C + 1, !,
    numeros_vistos(L, X, C1, N).
numeros_vistos([_|L], M, C, N) :-
    numeros_vistos(L, M, C, N).

% Predicado que invierte una lista.
invertir([X],[X]).
invertir([X|L1],L2) :- 
    invertir(L1,L3),
	afegir(L3,[X],L2).

% Predicado que añade una lista a otra.
afegir([],L,L).
afegir([X|L1],L2,[X|L3]):-
    afegir(L1,L2,L3).

% Predicado que obtiene la traspuesta de una lista de listas.
traspuesta([],[]).
traspuesta([[]|_], []):-!.
traspuesta([S|R], [L|L1]) :-
    traspuesta(S, R, L, M),
    traspuesta(M, L1).

% Predicado auxiliar que obtiene la traspuesta de una lista de listas.
traspuesta([], _,[],[]).
traspuesta([S1|S2], [], [S1|L1], [S2|M]):-
    traspuesta([], [], L1, M).
traspuesta([S1|S2], [R1|R2], [S1|L1], [S2|M]):-
    traspuesta(R1, R2, L1, M).

% Predicado que genera una lista de números del I al N.
rellenar(I,I,[I]).
rellenar(I,N,[I|L]):-
    I < N,
    I1 is I+1,
    rellenar(I1,N,L).

% Predicado que genera todas las permutaciones de una lista.
permuta([], []).
permuta([X|L1], L) :-
    permuta(L1, L2),
    insereix(X,L2,L).

% Predicado que inserta un elemento en todas las posiciones de una lista.
insereix(E,L,[E|L]).
insereix(E,[X|L1],[X|L2]):- insereix(E,L1,L2).