% kenken/3 
% N: num cells on each side of KenKen square
% C: list of numeric cage contraints
% T: list of list of integers; all length N; NxN grid 

list_Len(N, T):- %predicate to use length to be used in maplist
	length(T, N). 

list_dom(N, T):-
	fd_domain(T, 1, N).

kenken(N, C, T):- 
	length(T, N), %creates a list of N elements
	maplist(list_Len(N), T), %applies the length predicate to every element of list 
	maplist(fd_all_different, T), %makes sure all elements of T is different
	%only makes sure that [X,Y] is different, not the vice versa of it
	maplist(list_dom(N), T), %the domain for each list in the list is 1 - N 
	all_c(T,0,N), %gets the columns
	maplist(constraints(T), C),
	maplist(fd_labeling, T). %gives all possible solutions one at a time

all_c(_,N,N).
all_c(T,IndStart, N):-
	Ind #= IndStart + 1,
	cols_diff(T,IndStart),
	all_c(T,Ind,N).

%makes sure the columns elements are different
cols_diff(T,I):-
	brettsFunc(T,Ts,I),
	fd_all_different(Ts).

%from Brett's OH
%gets the columns
brettsFunc([],[],_).
brettsFunc([H|T],[FH|R],Nth):-
	nth0(Nth,H,FH),
	brettsFunc(T,R,Nth).

%get matrix elements
get_Matr_El(T,[I|J],El):-
	Is #= I-1,  %because nth is starting at 0
	Js #= J-1, 
	nth0(Is, T, R), %get the row
	nth0(Js, R ,El). %get the element in that row

%constraints
constraints(T, C):-
	get_con(T,C).
get_con(T, +(S, L)):- 
	add(T, S, L, 0).
get_con(T, *(P,L)):-
	mult(T, P, L, 1).
get_con(T, -(D,J,K)):-
	sub(T, D, J, K). 
get_con(T, /(Q,J,K)):-
	div(T, Q, J, K).

%% +(S, L)
%% *(P, L)
%% −(D, J, K) subtraction can only be 2 cells
%% /(Q, J, K) division can only be 2 cells
add(_, Sum,[], Sum). %sum is sum if L is empty
add(T, Sum, [HD|TL], Ans):-
	get_Matr_El(T, HD, El),
	Temp_Ans #= El + Ans,
	add(T, Sum, TL, Temp_Ans).

mult(_, M,[], M). %sum is sum if L is empty
mult(T, M, [HD|TL], Ans):-
	get_Matr_El(T, HD, El),
	Temp_Ans #= El * Ans,
	mult(T, M, TL, Temp_Ans).

sub(T, Dif, J, K):-
	get_Matr_El(T, J, J_El),
	get_Matr_El(T, K, K_El),
	(Dif #= J_El-K_El;
	Dif #= K_El-J_El).

div(T, Quo, J, K):-
	get_Matr_El(T, J, J_El),
	get_Matr_El(T, K, K_El),
	(Quo #= J_El/K_El;
	Quo #= K_El/J_El).

%plain kenken here
%brett said it is olay to use #= here
create_domL(N, T):-  %creates a list of 1 - N
	get_doms(N, [], T). 

get_doms(0,T,T):-!. %gets the numbers 1 - N 
get_doms(N, D, T):- 
	Ns #= N-1, 
	get_doms(Ns, [N|D], T).  

get_row(N, T) :-  %gets list of 1 - N
    create_domL(N, T).
    check_dom(NT).

check_dom(_,[]):- !.  %checks domain is 1 - N
check_dom(N,[Hs|Tl]):-
	(Hs #>= 1), 
	(Hs #=< N), 
	check_dom(N,Tl).

all_diff([]).
all_diff([Hd|Tl]):- %makes sure all elements are diff
	\+ (member(Hd, Tl)), %uses member and \+ to prove that head is not in tail; 
	all_diff(Tl). 

plain_all_c(_,N,N). %same as all_c aboves; gets the columns/tranpose
plain_all_c(T,IndStart, N):-
	Ind #= IndStart + 1,
	cols_diff_plain(T,IndStart),
	all_c(T,Ind,N).

cols_diff_plain(T,I):-
	brettsFunc(T,Ts,I),
	all_diff(Ts).

plain_kenken(N, C, T):-
	length(T, N), %creates a list of N elements
	maplist(list_Len(N), T), %applies the length predicate to every element of list 
	get_row(N, Perm), 
	maplist(permutation(Perm), T),
	plain_all_c(T, 0, N), 	
	maplist(constraints(T), C).

%% %from Brett's discussion
%% tdarr(_, []). %checks if every el in list is list of length Num
%% tdarr(Num, [X|XS]):-
%% 	length(X,Num), 
%% 	tdarr(Num, XS). 




