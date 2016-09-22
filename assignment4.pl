%input must be in list form:
drroberts:-
write('What do you want to tell me?'), 
    read(Input), 
   	transform(Input, Output),
    write(Output),
    write(.).

% These methods are created purely to match the expected predicates in the handout.
% They are duplicates and will produce the same result as simply calling drroberts.
printSentence(Input):- write(Input).
answer(Input, X):- transform(Input, X).

% Transform pattern matches to see which word pattern is being used, searches for appropriate
% replacements, adds a qm to the end and returns the new list:
transform([i, want, to |T], [why, do, you, want, to |Y]):-transform(T,Y).
transform([i, X , about |T], [have, you, ever, Z, about, T ,before, qm]):- tensechange(X, Z).
%a more specific reply for feelings.
transform([i,feel |T], [what, makes, you, feel, Y]):- transform(T,Y).
% a more specific response to know.
transform([i,know, i, am |T], [[are, you, sure, you, know, that, you, are]|Y]):- transform(T,Y).
transform([i, am, X|T], [why, are, you, X | Y]):- transform(T,Y).
%this is the default reaction to a statement:
transform([i, X, R| T], [why, do, you, X, R2 | Y]):- transform(T,Y), relation(R, R2).
transform([X|T],[X2| Y]):- relation(X, X2),transform(T,Y).
% add a qm to the end of the output.
transform([], [qm]).

%Changes the person talking.
relation(my, your).
relation(your, my).
relation(am, are).
relation(X, X).
% This is the default for relationships we either havent seen before
% or do not need to alter: his, hers, ours etc.

% Changes the words that are still needed in the sentence but need altering.
tensechange(feel, felt).
tensechange(know, knew).
%if there is an e as the last letter, just add a d
% otherwise add ed
tensechange(A, X):-(sub_atom(A, _, 1, 0, e) -> atom_concat(A, d, X); atom_concat(A,ed, X)).


% -----Tests-----
% The first sets do not reversly produce an input, the later ones do. 
% examples of [i, _, about, _].
:- transform([i, fantisise, about, fast, cars], [have, you, ever, fantisised, about, [fast, cars], before, qm]).
:- transform([i, dream, about, prolog],[have, you, ever, dreamed, about, [prolog], before, qm]).
% This example does not go backwards as the tensechange predicate does not know whether it should take
% d or ed off the end. 

% The following examples will reversly produce an input list. I think this is because it is constructed in such a way\
% that there are no tense changes. 

% examples of [i, feel, _, about _].
:- transform([i, feel, happy, about, my, brother],[what, makes, you, feel, [happy, about, your, brother, qm]]).
:- transform([i, feel, bad, about, harambe], [what, makes, you, feel, [bad, about, harambe, qm]]).
% transform(X, [what, makes, you, feel, [bad, about, harambe, qm]]).--just run this to see a reversal

% exampless of [i, _, _].
:- transform([i,want,to,cry], [why, do, you, want, to, cry, qm]).
:- transform([i, want, to , go , to, france], [why, do, you, want, to, go, to, france, qm]).
:- transform([i, feel, sad], [what, makes, you, feel,[ sad, qm]]).
:- transform([i, like, his, dog], [why, do, you, like, his, dog, qm]).%other person example
:- transform([i, like, my, cat], [why, do, you, like, your, cat, qm]). % my/your change example
% transform(X, [what, makes, you, feel,[ sad, qm]]).--just run this to see a reversal

% examples of [i, know, i, am _].
:-transform([i, know, i, am, insecure], [[are, you, sure, you, know, that, you, are], insecure, qm]).
:-transform([i, know, i, am, really, cool], [[are, you, sure, you, know, that, you, are], really, cool, qm]).
%transform(X, [[are, you, sure, you, know, that, you, are], really, cool, qm]).--just run this to see a reversal

% examples of [i, am, _]:
:- transform([i,am, harambe], [why, are, you, harambe, qm]).
:- transform([i, am, happy], [why, are, you, happy, qm]).
%transform(X, [why, are, you, happy, qm]).--just run this to see a reversal