% ---------------------------------------------------------------------------------------------------------
% By Oliver Barnes - 1905121
%
% Rules and lexicon are at the top of the file, test queries and output are at the bottom.
% Should be complete according to all given rules.
% Some queries return same parse twice, I think it's due to duplicate rules or rule overlap.
% ---------------------------------------------------------------------------------------------------------


% ---------------------------------------------------------------------------------------------------------
%			               __         
%			   _______  __/ /__  _____
%			  / ___/ / / / / _ \/ ___/
%			 / /  / /_/ / /  __(__  ) 
%			/_/   \__,_/_/\___/____/  
%                          
% ---------------------------------------------------------------------------------------------------------

% sentence rules ------------------------------------------------------------------------------------------

% most sentences take the form subject, verb, object (only exception is for intransitive verbs)
s(s(NP,VP))-->np(common,singular,ANIM,NP),vp(singular,_,ANIM,VP).
s(s(NP,VP))-->np(common,plural,ANIM,NP),vp(plural,_,ANIM,VP).

% use of pronouns assumes animacy
s(s(NP,VP))-->np(pronoun,singular,subject,NP),vp(singular,_,_,VP).
s(s(NP,VP))-->np(pronoun,plural,subject,NP),vp(plural,_,_,VP).

% phrase rules --------------------------------------------------------------------------------------------

%np - noun phrase
np(common,X,ANIM,np(DET,NBAR))-->det(X,DET),nbar(X,ANIM,NBAR).
%pronoun phrases
np(pronoun,X,subject,np(PN))-->pro(X,subject,PN).
np(pronoun,X,object,np(PN))-->pro(X,object,PN).
%np with preposition
np(common,X,ANIM,np(DET,NBAR,PP))-->det(X,DET),nbar(X,ANIM,NBAR),pp(PP).
np(pronoun,X,subject,np(PN,PP))-->pro(X,subject,PN),pp(PP).
np(pronoun,X,object,np(PN,PP))-->pro(X,object,PN),pp(PP).

%vp - verb phrase
vp(X,transitive,ANIM,vp(V,NP))-->v(X,transitive,ANIM,V),np(common,_,_,NP).
%only object pronouns can be used within verb phrases
vp(X,transitive,ANIM,vp(V,NP))-->v(X,transitive,ANIM,V),np(pronoun,_,object,NP).
%intransitive verbs don't need noun phrases
vp(X,intransitive,ANIM,vp(V))-->v(X,intransitive,ANIM,V).

%jp - adjective phrase
jp(X,ANIM,jp(ADJ,N))-->adj(ADJ),n(X,ANIM,N).
jp(X,ANIM,jp(ADJ,JP))-->adj(ADJ),jp(X,ANIM,JP).

%pp - prepositional phrase
pp(pp(PREP,NP)) --> prep(PREP),np(common,_,_,NP).
pp(pp(PREP,NP)) --> prep(PREP),np(pronoun,_,object,NP).
pp(pp(PREP,NP,NPP)) --> prep(PREP),np(common,_,_,NP),pp(NPP).
pp(pp(PREP,NP,NPP)) --> prep(PREP),np(pronoun,_,object,NP),pp(NPP).

%nbar - still not 100% sure. I think it's for common nouns, so that they can possess adjectives.
nbar(X,ANIM,nbar(N))-->n(X,ANIM,N).
nbar(X,ANIM,nbar(JP))-->jp(X,ANIM,JP).

% word classifiers ----------------------------------------------------------------------------------------

% X - plural/singular
det(X,det(Word)) --> [Word], {lex(Word,det,X)}.
% X - plural/singular | ANIM  - animacy
n(X,ANIM,n(Word)) --> [Word], {lex(Word,noun,X,ANIM)}.
% X - plural/singular | Y - transitive/intransitive | ANIM - animacy
v(X,Y,ANIM,v(Word)) --> [Word], {lex(Word,verb,X,Y,ANIM)}.
% X - plural/singular | Y - subject/object 
pro(X,Y,pro(Word)) --> [Word], {lex(Word,pronoun,X,Y)}.
%
adj(adj(Word)) --> [Word], {lex(Word,adj)}.
%
prep(prep(Word)) --> [Word], {lex(Word,prep)}.

% lexicon -------------------------------------------------------------------------------------------------
%DETERMINERS ----------------------------------------------------------------------------------------------

lex(the,det,_).
lex(a,det,singular).
lex(two,det,plural).

%NOUNS ----------------------------------------------------------------------------------------------------

% anim property refers to animacy

lex(man,noun,singular,anim).
lex(woman,noun,singular,anim).
lex(men,noun,plural,anim).
lex(women,noun,plural,anim).
lex(chair,noun,singular,inanim).
lex(chairs,noun,plural,inanim).
lex(apple,noun,singular,inanim).
lex(apples,noun,plural,inanim).
lex(room,noun,singular,inanim).
lex(rooms,noun,plural,inanim).

%PRONOUNS -------------------------------------------------------------------------------------------------

% all pronouns assume animacy

% "i" & "you" categorised as a plural subject pronoun as it makes things a lot easier.
% means that I didn't have to implement grammatical person.
% their uses are closer to that of "they" than "he" or "she" as they are first and second person pronouns.
% e.g. "he falls", "she falls", "they fall", "i fall".
% also prevents phrases like "i hires her" from being valid.
lex(i,pronoun,plural,subject).
lex(you,pronoun,plural,subject).
lex(we,pronoun,plural,subject).
lex(they,pronoun,plural,subject).
lex(he,pronoun,singular,subject).
lex(she,pronoun,singular,subject).
lex(it,pronoun,singular,subject).

% there are cases where some of these pronouns (e.g. "them") could be used
% as plural or singular. excluded for brevity, doesn't affect program output.
lex(me,pronoun,singular,object).
lex(you,pronoun,singular,object).
lex(him,pronoun,singular,object).
lex(her,pronoun,singular,object).
lex(it,pronoun,singular,object).
lex(them,pronoun,plural,object).
lex(us,pronoun,plural,object).

%VERBS ----------------------------------------------------------------------------------------------------

% anim property refers to animacy
% animacy only considered in a basic way (only subject animacy considered, 
% "the man hires the apple" is considered correct under this grammar).

lex(hires,verb,singular,transitive,anim).
lex(hire,verb,plural,transitive,anim).
lex(sees,verb,singular,transitive,anim).
lex(see,verb,plural,transitive,anim).
lex(knows,verb,singular,transitive,anim).
lex(know,verb,plural,transitive,anim).

lex(falls,verb,singular,intransitive,_).
lex(fall,verb,plural,intransitive,_).
lex(sleeps,verb,singular,intransitive,anim).
lex(sleep,verb,plural,intransitive,anim).

%ADJECTIVES -----------------------------------------------------------------------------------------------

% didn't implement adjective order ("big red car" is correct while "red big car" sounds weird).

lex(short,adj).
lex(tall,adj).
lex(old,adj).
lex(young,adj).
lex(red,adj).

%PREPOSITION ----------------------------------------------------------------------------------------------

% didn't implement anything to identify correct preposition ("on the room" are just as valid as 
% "in the room", while both are technically valid(?), one is more likely).

lex(in,prep).
lex(on,prep).
lex(under,prep).


% ---------------------------------------------------------------------------------------------------------
%			               __              __ 
%			  ____  __  __/ /_____  __  __/ /_
%			 / __ \/ / / / __/ __ \/ / / / __/
%			/ /_/ / /_/ / /_/ /_/ / /_/ / /_  
%			\____/\__,_/\__/ .___/\__,_/\__/  
%			              /_/                               
%
% ---------------------------------------------------------------------------------------------------------

% sentence:	
% query:	
% parse:	

% sentence:	the woman sees the apples
% query:	s(Tree,[the,woman,sees,the,apples],[]).
% parse:	Tree = s(np(det(the), nbar(n(woman))), vp(v(sees), np(det(the), nbar(n(apples))))) 

% sentence:	a woman knows him
% query:	s(Tree,[a,woman,knows,him],[]).
% parse:	Tree = s(np(det(a), nbar(n(woman))), vp(v(knows), np(pro(him)))) 

% sentence:	two woman hires a man
% query:	s(Tree,[two,woman,hires,a,man],[]).
% parse:	false.

% sentence:	two women hire a man
% query:	s(Tree,[two,women,hire,a,man],[]).
% parse:	Tree = s(np(det(two), nbar(n(women))), vp(v(hire), np(det(a), nbar(n(man)))))

% sentence:	she knows her
% query:	s(Tree,[she,knows,her],[]).
% parse:	Tree = s(np(pro(she)), vp(v(knows), np(pro(her)))) 

% sentence:	she know the man
% query:	s(Tree,[she,know,the,man],[]).
% parse:	false.

% sentence:	us see the apple
% query:	s(Tree,[us,see,the,apple],[]).
% parse:	false.

% sentence:	we see the apple
% query:	s(Tree,[we,see,the,apple],[]).
% parse:	Tree = s(np(pro(we)), vp(v(see), np(det(the), nbar(n(apple))))) 

% sentence:	i know a short man
% query:	s(Tree,[i,know,a,short,man],[]).
% parse:	Tree = s(np(pro(i)), vp(v(know), np(det(a), nbar(jp(adj(short), n(man)))))) 

% sentence:	he hires they 
% query:	s(Tree,[he,hires,they],[]).
% parse:	false.

% sentence:	two apples fall
% query:	s(Tree,[two,apples,fall],[]).
% parse:	Tree = s(np(det(two), nbar(n(apples))), vp(v(fall)))

% sentence:	the apple falls 
% query:	s(Tree,[the,apple,falls],[]).
% parse:	s(np(det(the), nbar(n(apple))), vp(v(falls)))

% sentence:	the apples fall
% query:	s(Tree,[the,apples,fall],[]).
% parse:	Tree = s(np(det(the), nbar(n(apples))), vp(v(fall))) 

% sentence:	i sleep 
% query:	s(Tree,[i,sleep],[]).
% parse:	Tree = s(np(pro(i)), vp(v(sleep))) 

% sentence:	you sleep
% query:	s(Tree,[you,sleep],[]).
% parse:	̀Tree = s(np(pro(you)), vp(v(sleep))) ;

% sentence:	she sleeps
% query:	s(Tree,[she,sleeps],[]).
% parse:	Tree = s(np(pro(she)), vp(v(sleeps))) 

% sentence:	he sleep
% query:	s(Tree,[he,sleep],[]).
% parse:	false.

% sentence:	them sleep
% query:	s(Tree,[them,sleep],[]).
% parse:	false.

% sentence:	a men sleep
% query:	s(Tree,[a,men,sleep],[]).
% parse:	false.

% sentence:	the tall woman sees the red
% query:	s(Tree,[the,tall,woman,sees,the,red],[]).
% parse:	false.

% sentence:	the young tall man knows the old short woman
% query:	s(Tree,[the,tall,young,man,knows,the,old,short,woman],[]).
% parse:	Tree = s(np(det(the), nbar(jp(adj(tall), jp(adj(young), n(man))))), vp(v(knows), np(det(the), nbar(jp(adj(old), jp(adj(short), n(woman)))))))

% sentence:	a man tall knows the short woman
% query:	s(Tree,[a,man,tall,knows,the,short,woman],[]).
% parse:	false.

% sentence:	a man on a chair sees a woman in a room
% query:	s(Tree,[a,man,on,a,chair,sees,a,woman,in,a,room],[]).
% parse:	Tree = s(np(det(a), nbar(n(man)), pp(prep(on), np(det(a), nbar(n(chair))))), vp(v(sees), np(det(a), nbar(n(woman)), pp(prep(in), np(det(a), nbar(n(room)))))))

% sentence:	a man on a chair sees a woman a room in
% query:	s(Tree,[a,man,on,a,chair,sees,a,woman,a,room,in],[]).
% parse:	false.

% sentence:	the tall young woman in a room on the chair in a room in the room sees the red apples under the chair
% query:	s(Tree,[the,tall,young,woman,in,a,room,on,the,chair,in,a,room,in,the,room,sees,the,red,apples,under,the,chair],[])
% ////////// SWI prolog didn't put out a complete parse for this, substituted some expressions for [...]. It also gave about 5 variations of the parse.
% parse:	Tree = s(np(det(the), nbar(jp(adj(tall), jp(adj(young), n(woman)))), pp(prep(in), np(det(a), 
%					nbar(n(room)), pp(prep(on), np(det(the), nbar(n(chair)), pp(prep(in), np(det(a), nbar(n(...)), 
%					pp(prep(...), np(..., ...))))))))), vp(v(sees), np(det(the), nbar(jp(adj(red), n(apples))), 
%					pp(prep(under), np(det(the), nbar(n(chair)))))))

% sentence:	the woman sees the apples
% query:	s(Tree,[the,woman,sees,the,apples],[]).
% parse:	Tree = s(np(det(the), nbar(n(woman))), vp(v(sees), np(det(the), nbar(n(apples)))))

% sentence:	a woman knows him
% query:	s(Tree,[a,woman,knows,him],[]).
% parse:	Tree = s(np(det(a), nbar(n(woman))), vp(v(knows), np(pro(him))))

% sentence:	the man sleeps
% query:	s(Tree,[the,man,sleeps],[]).
% parse:	Tree = s(np(det(the), nbar(n(man))), vp(v(sleeps)))

% sentence:	the room sleeps
% query:	s(Tree,[the,room,sleeps],[]).
% parse:	false.

% sentence:	the apple sees the chair
% query:	s(Tree,[the,apple,sees,the,chair],[]).
% parse:	false.

% sentence:	the rooms know the man
% query:	s(Tree,[the,rooms,know,the,man],[]).
% parse:	false.

% sentence:	the apple falls
% query:	s(Tree,[the,apple,falls],[]).
% parse:	Tree = s(np(det(the), nbar(n(apple))), vp(v(falls))) 

% sentence:	the man falls
% query:	s(Tree,[the,man,falls],[]).
% parse:	Tree = s(np(det(the), nbar(n(man))), vp(v(falls))) 