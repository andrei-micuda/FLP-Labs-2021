sits_right_of('Tywin Lannister', 'Petyr Baelish').
sits_right_of('Petyr Baelish', 'Varys').
sits_right_of('Varys', 'Grand Maester Pycelle').
sits_right_of('Grand Maester Pycelle', 'Tyrion Lannister').
sits_right_of('Tyrion Lannister', 'Janos Slynt').
sits_right_of('Janos Slynt', 'Cersei Baratheon').
sits_right_of('Cersei Baratheon', 'Tywin Lannister').

sits_left_of(X, Y) :- sits_right_of(Y, X).

are_neighbors_of(X, Y, Z) :- sits_left_of(X, Z), sits_right_of(Y, Z).

next_to_each_other(X, Y) :- sits_right_of(X, Y).
next_to_each_other(X, Y) :- sits_left_of(X, Y).

/** <examples>
?- sits_right_of('Petyr Baelish', 'Cersei Baratheon').
?- sits_right_of('Petyr Baelish', 'Varys').
?- sits_right_of(X, 'Janos Slynt').
?- sits_right_of(X, 'Cersei Baratheon'), sits_right_of(Y, X).
?- are_neighbors_of('Petyr Baelish', 'Grand Maester Pycelle', Z);are_neighbors_of('Grand Maester Pycelle', 'Petyr Baelish', Z).
*/

%---------------------------------
% Jon Snow and Daenerys Targaryen
%---------------------------------

male(rickardStark).
male(eddardStark).
male(brandonStark).
male(benjenStark).
male(robbStark).
male(branStark).
male(rickonStark).
male(jonSnow).
male(aerysTargaryen).
male(rhaegarTargaryen).
male(viserysTargaryen).
male(aegonTargaryen).

%---------------------------

female(lyarraStark).
female(catelynStark).
female(lyannaStark).
female(sansaStark).
female(aryaStark).
female(rhaellaTargaryen).
female(eliaTargaryen).
female(daenerysTargaryen).
female(rhaenysTargaryen).

%---------------------------

person(X) :- male(X);female(X).

%---------------------------

parent_of(rickardStark,eddardStark).
parent_of(rickardStark,brandonStark).
parent_of(rickardStark,benjenStark).
parent_of(rickardStark,lyannaStark).
parent_of(lyarraStark,eddardStark).
parent_of(lyarraStark,brandonStark).
parent_of(lyarraStark,benjenStark).
parent_of(lyarraStark,lyannaStark).

parent_of(eddardStark,robbStark).
parent_of(eddardStark,sansaStark).
parent_of(eddardStark,aryaStark).
parent_of(eddardStark,branStark).
parent_of(eddardStark,rickonStark).
parent_of(catelynStark,robbStark).
parent_of(catelynStark,sansaStark).
parent_of(catelynStark,aryaStark).
parent_of(catelynStark,branStark).
parent_of(catelynStark,rickonStark).

parent_of(aerysTargaryen,rhaegarTargaryen).
parent_of(aerysTargaryen,viserysTargaryen).
parent_of(aerysTargaryen,daenerysTargaryen).

parent_of(rhaellaTargaryen,rhaegarTargaryen).
parent_of(rhaellaTargaryen,viserysTargaryen).
parent_of(rhaellaTargaryen,daenerysTargaryen).

parent_of(rhaegarTargaryen,jonSnow).
parent_of(lyannaStark,jonSnow).

parent_of(rhaegarTargaryen,aegonTargaryen).
parent_of(rhaegarTargaryen,rhaenysTargaryen).

parent_of(eliaTargaryen,aegonTargaryen).
parent_of(eliaTargaryen,rhaenysTargaryen).

%----------------------------
father_of(Father, Child) :- parent_of(Father, Child), male(Father).

mother_of(Mother, Child) :- parent_of(Mother, Child), female(Mother).

grandfather_of(Grandfather, Child) :- parent_of(Parent, Child), father_of(Grandfather, Parent).

grandmother_of(Grandmother, Child) :- parent_of(Parent, Child), mother_of(Grandmother, Parent).

sister_of(Sister, Person) :- female(Sister), parent_of(X, Sister), parent_of(X, Person), Sister \= Person.        

brother_of(Brother, Person) :- male(Brother), parent_of(X, Brother), parent_of(X, Person), Brother \= Person.

aunt_of(Aunt, Person) :- female(Aunt), parent_of(X, Person), sister_of(Aunt, X).

uncle_of(Uncle, Person) :- male(Uncle), parent_of(X, Person), brother_of(Uncle, X).

ancestor_of(Ancestor, Person) :- parent_of(Ancestor, Person);
    							 parent_of(X, Person), ancestor_of(Ancestor, X).

not_parent(X, Y) :- person(X), parent_of(Z, Y), Z \= X.

ancestor_not_parent(X, Y) :- ancestor_of(X, Y), parent_of(Z, Y), X \= Z.

% --------------------------------------------------

/** GOT riddle 
Varys - "Power is a curious thing, my lord. Are you fond of riddles?" 
Tyrion - "Why? Am I about to hear one?" 
Varys - "Three great men sit in a room, a king, a priest and the rich man. 
         Between them stands a common sellsword. 
         Each great man bids the sellsword kill the other two. 
         Who lives? Who dies?" 
Tyrion - "Depends on the sellsword" 
*/


char(king).
char(priest).
char(richMan).


choice(god, priest).
choice(authority, king).
choice(wealth, richMan).

is_killed(C, X, Y) :- choice(C, S), char(X), char(Y), X \= S, Y \= S, X \= Y.