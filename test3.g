RandomColumnVector := function( args... )
	local dim, range, func, mat;

	dim := args[1]; range := args[2];
	if Length(args) >= 3 then
		func:= args[3];
	elif Length(args) <3 then
		func := x->true;
	fi;

    repeat 
        mat := List( [1..dim], x-> List( [1..1], x->Random( range )));
    until func( mat );		

	return mat;
end;

Q1 := function()

    local mat, eigs, wronganswerrange, p1, p2, title, qtext, answers, defgrade, 
            penalty, single, shuffle, tags, qrec;

    repeat 
        mat := RandomMatrix( 2, [-2..2 ]);
        eigs := Eigenvalues( Rationals, mat );
    until Length( eigs ) = 2 and IsInt( eigs[1] ) and IsInt( eigs[2] );

    wronganswerrange := [-10..10];
    p1 := Position( wronganswerrange, eigs[1] );
    p2 := Position( wronganswerrange, eigs[2] );
    
    if p1 <> fail then Remove( wronganswerrange, p1 ); fi;
    if p2 <> fail then Remove( wronganswerrange, p2 ); fi;

    title := "Escolha os autovalores";
    
    qtext := Concatenation( "Quais são autovalores da matriz \\(", LaTeXObj( mat ), 
                            "\\)?" );
    answers := [[eigs[1], 50], 
                [eigs[2], 50], 
                [Random( wronganswerrange ), -50 ],
                [Random( wronganswerrange ), -50 ]];

    defgrade := 1;
    penalty := "0.1";
    single := false;
    shuffle := 1;
    tags := [ "Q1", "Csaba" ];

    qrec := rec( 
            title := title,
            type := "multi",
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle, 
            tags := tags );

    return qrec;
end;

Q2 := function()

    local mat, eigs, wronganswerrange, goodmats, badmats, allmats, matrixlist, X, numbercorrect, marks, title, qtext, answers, defgrade, 
            penalty, single, shuffle, tags, qrec;

    goodmats:= [];
    badmats:=[];
 
    repeat
        repeat 
            mat := RandomSymmetricMatrix( 3, [ 1..6 ]);
            eigs := Eigenvalues( Rationals, mat );
        until Length( eigs ) = 3 and IsInt( eigs[1] ) and 
                IsInt( eigs[2] ) and IsInt( eigs[2] );

    	if eigs[1] > 0 and 
            eigs[2] > 0 and eigs[3] > 0 and 
            Length( Set(goodmats) ) < 3 then 
                Add( goodmats, mat ); 
        elif Length( Set(badmats) ) < 3 then 
            Add( badmats, mat ); 
        fi;

    until Length( Set(goodmats) ) = 3 and Length( Set(badmats) ) = 3;
	
    allmats := Set(goodmats);
    UniteSet( allmats, Set( badmats ));

    numbercorrect:=1;

    X := Random(goodmats);
    matrixlist := [ [X,1] ];
    RemoveSet(allmats, X);

    X := Random(allmats);
    if X in goodmats then 
        numbercorrect := numbercorrect+1;
        Add( matrixlist, [X,1]);
    else 
        Add( matrixlist, [X,2]); 
    fi;
    RemoveSet(allmats, X);
    
    X:=Random(allmats);
    if X in goodmats then 
        numbercorrect := numbercorrect+1;
        Add( matrixlist, [X,1]);
    else 
        Add( matrixlist, [X,2]); 
    fi;
    RemoveSet(allmats, X);

    X:=Random(allmats);
    if X in goodmats then 
        numbercorrect:=numbercorrect+1;
        Add( matrixlist, [X,1]);
    else 
        Add( matrixlist, [X,2]); 
    fi;
    RemoveSet(allmats, X);

    marks:=[0,0];
    if numbercorrect = 1 then marks[1]:="100"; marks[2]:="-33.333";fi;
    if numbercorrect = 2 then marks[1]:="50"; marks[2]:="-50";fi;
    if numbercorrect = 3 then marks[1]:="33.333"; marks[2]:="-100";fi;

    title := "Matriz eh IP?";
    
    qtext :=  "Marque as matrizes \\(A\\) com respeito a quais a forma bilinear \\(\\langle -,-\\rangle_A\\) é um produto interno";
    answers := [ [ Concatenation("\\(",LaTeXObj(matrixlist[1][1]),"\\)"), 
                                marks[matrixlist[1][2]]], 
                 [ Concatenation("\\(",LaTeXObj(matrixlist[2][1]),"\\)"), 
                                marks[matrixlist[2][2]]], 
                 [ Concatenation("\\(",LaTeXObj(matrixlist[3][1]),"\\)"), 
                                marks[matrixlist[3][2]]],
                 [ Concatenation("\\(",LaTeXObj(matrixlist[3][1]),"\\)"), 
                                marks[matrixlist[4][2]]]];

    defgrade := 1;
    penalty := "0.1";
    single := false;
    shuffle := 1;
    tags := [ "Q2", "John" ];

    qrec := rec( 
            title := title,
            type := "multi", 
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle,
            tags := tags );

    return qrec;

end;

Q3 := function()

    local qrec, subquestions,mats; 

    repeat 
        mats := List( [1..4], x->RandomMatrix( 2, [-5..5] ));
        mats := Set( mats );
    until Length( mats ) = 4;

    subquestions := List( [1..4], x->[ Concatenation("\\(",LaTeXObj(mats[x]),"\\)"), 
                                  Determinant( mats[x] )]);

    qrec := rec(
            title := "Combine com determinante",
            type := "matching",
            qtext := "Combine a matriz com seu determinante", 
            subquestions := subquestions, 
            defgrade := "1", 
            penalty := "0.1", 
            shuffle := "1", 
            tags := [ "Q3", "match" ]); 
    
    return qrec;
end;






W10Q1 := function()

	local qrec, tags, subquestions, mats, matsSet, X, s, t, i, v, matbadveclist, list1, list2, mmat;

	
	t:=1;
	matbadveclist:=[];


	# I'm here.  This while loop yields matbadveclist, which is a list like [[mat1,vec1] , [mat2,vec2] , [mat3,vec3]]
	# where each mati is a symmetric non-PD matrix M and the corresponding veci is a column matrix v such that
	# v^t.M.v <= 0 but such that v^t.mati.v > 0 for the other two matrices.

	while t=1 do
#		matbadveclist := [];
		t:=0;

		mats:=[];
		repeat
			X:= RandomSymmetricMatrix( 3, [-5..5] );
			if not IsPositiveDefinite(X) then Add(mats,X);fi;
			matsSet:=Set(mats);
		until Length( matsSet ) = 3;

		s:=0;	
		for i in [1..1000] do
			if s=0 then
				v:=RandomColumnVector(3,[1..7]); 
				if (TransposedMat(v)*mats[1]*v)[1][1] <=0 and (TransposedMat(v)*mats[2]*v)[1][1] >0 and (TransposedMat(v)*mats[3]*v)[1][1] >0 then
					matbadveclist := [ [mats[1], v] ]; s:=1;
				fi;	
			fi;	
		od;			

		if Length(matbadveclist) = 0 then t:=1; fi;

		s:=0;
		if t=0 then
		for i in [1..1000] do
			if s=0 then
				v:=RandomColumnVector(3,[1..7]); 
				if (TransposedMat(v)*mats[2]*v)[1][1] <=0 and (TransposedMat(v)*mats[1]*v)[1][1] >0 and (TransposedMat(v)*mats[3]*v)[1][1] >0 then
					Add( matbadveclist,  [mats[2], v] ); s:=1;
				fi;	
			fi;
		od;
		fi;	

		if Length(matbadveclist) = 1 then t:=1; fi;

		s:=0;
		if t=0 then
		for i in [1..1000] do
			if s=0 then
				v:=RandomColumnVector(3,[1..7]); 
				if (TransposedMat(v)*mats[3]*v)[1][1] <=0 and (TransposedMat(v)*mats[1]*v)[1][1] >0 and (TransposedMat(v)*mats[2]*v)[1][1] >0 then
					Add( matbadveclist, [mats[3], v] ); s:=1;
				fi;	
			fi;
		od;
		fi;	

		if Length(matbadveclist) = 2 then t:=1; fi;
		
	od;

	mmat := List( matbadveclist, x->List( matbadveclist, 
			y->TransposedMat( y[2] )*x[1]*y[2] ));
	
	Print( mmat, "\n" );
	if mmat[1,2][1,1] <= 0 or mmat[1,3][1,1] <= 0 or mmat[2,1][1,1] <= 0 or 
			mmat[2,3][1,1] <= 0 
			or mmat[3,1][1,1] <= 0 or mmat[3,2][1,1] <= 0 then 
			Error( "wrong vectors" );
	fi;

	list1 := List( matbadveclist, x->x[1] ); 

	list2 := List( matbadveclist, x->TransposedMat(x[2]) );
	#for i in [1..3] do
	#	v:=list2[i];
	#	v := ReplacedString( String( v ), "[ [", "(" ); v := ReplacedString( v, "] ]", #")");
	#	list2[i]:=v;
	#od;

   	 tags := [ "W10Q1" ];

	return [list1, List( list2, x->x[1] )];	

return MoodleMatchingQuestionByLists(list1, list2, "match matriz with bad vector", "Junte cada matriz com um vetor que pode ser usado \n para ver que a forma bilinear \\(\\langle -,-\\rangle_A\\) não é um produto interno.", rec(tags:=tags));

end;











RandomPDMatrix := function( args... )

	local dim, range, A, X; 

	A:=1;
	dim:=args[1];
	range:=args[2];
	

	while A=1 do
		X := RandomSymmetricMatrix(dim,range); if IsPositiveDefinite(X) then A:=X; fi;
	od;

return A;

end;





textvector := function (args...)
	local v;
	v:=args[1];
	
	v := ReplacedString( String( v ), "[ [", "(" ); v := ReplacedString( v, "] ]", ")");
	return v;
end;




W10Q2 := function( )

	local qrec, subquestions, A, X, v, i, firstvec, oknorms, wantednorm, goodvecs, badvecs, d, allvecs, veclist, numbercorrect, marks, rightnorm, title, qtext, answers, defgrade, 
            penalty, single, shuffle, tags;


	oknorms:=[1,4,9,16,25];
	firstvec:=1;

	   	
	while firstvec = 1 do
		A:=RandomPDMatrix(3,[-2..5]);
		v:=RandomColumnVector(3,[-2..4]);
		if (TransposedMat(v)*A*v)[1][1] in oknorms then firstvec:=v; wantednorm:=(TransposedMat(v)*A*v)[1][1]; fi;
	od;		

	# A is a random PD matrix, firstvec is a vector having norm squared wantednorm, which was chosen to 1,2,3,4 or 5.

	goodvecs:=[firstvec]; 
	badvecs:=[];

        	repeat 
			v := RandomColumnVector( 3, [ -2..5 ]);
	           	d := (TransposedMat(v)*A*v)[1][1];
			if d = wantednorm and Length( Set(goodvecs) ) < 3 then Add(goodvecs, v);fi;
			if not d = wantednorm and Length( Set(badvecs) ) < 3 then Add(badvecs, v);fi;
		until Length(goodvecs) = 3 and Length(badvecs) = 3;
	
	
	# goodvecs is a list of 3 vectors with norm squared wantednorm and badvecs is a list of 3 vectors with norm squared not wantednorm (norm wrt A)



	allvecs := Set(goodvecs);
	    UniteSet( allvecs, Set( badvecs ));

	    numbercorrect:=1;
	X := Random(goodvecs);
	    veclist := [ [X,1] ];
	    RemoveSet(allvecs, X);

	    X := Random(allvecs);
	    if X in goodvecs then 
	        numbercorrect := numbercorrect+1;
	        Add( veclist, [X,1]);
	    else 
	        Add( veclist, [X,2]); 
	    fi;
	    RemoveSet(allvecs, X);
    
	    X:=Random(allvecs);
	    if X in goodvecs then 
 	       numbercorrect := numbercorrect+1;
	        Add( veclist, [X,1]);
	    else 
	        Add( veclist, [X,2]); 
	    fi;
	    RemoveSet(allvecs, X);

 	   X:=Random(allvecs);
	    if X in goodvecs then 
 	       numbercorrect:=numbercorrect+1;
	        Add( veclist, [X,1]);
	    else 
	        Add( veclist, [X,2]); 
	    fi;
	    RemoveSet(allvecs, X);

		for i in [1..4] do
		veclist[i][1]:=TransposedMat(veclist[i][1]);
		od;



 	   marks:=[0,0];
	    if numbercorrect = 1 then marks[1]:="100"; marks[2]:="-33.333";fi;
 	   if numbercorrect = 2 then marks[1]:="50"; marks[2]:="-50";fi;
 	   if numbercorrect = 3 then marks[1]:="33.333"; marks[2]:="-100";fi;

		if wantednorm = 1 then rightnorm:=1;fi;
		if wantednorm = 4 then rightnorm:=2;fi;
		if wantednorm = 9 then rightnorm:=3;fi;
		if wantednorm = 16 then rightnorm:=4;fi;
		if wantednorm = 25 then rightnorm:=5;fi;

    title := "find vecs with right norm";
    
    qtext :=  Concatenation("Marque os vetores tendo norma ", "\\(", String( rightnorm ), "\\)", " com respeito ao \n produto interno \\(\\langle -,-\\rangle_A\\) dado pela matriz ", "$$A=", LaTeXObj( A ), ".$$");
 



answers := [ [ Concatenation("\\(",textvector(veclist[1][1]),"\\)"), 
                                marks[veclist[1][2]]], 
                 [ Concatenation("\\(",textvector(veclist[2][1]),"\\)"), 
                                marks[veclist[2][2]]], 
                 [ Concatenation("\\(",textvector(veclist[3][1]),"\\)"), 
                                marks[veclist[3][2]]],
                 [ Concatenation("\\(",textvector(veclist[4][1]),"\\)"), 
                                marks[veclist[4][2]]]];




    defgrade := 1;
    penalty := "0.1";
    single := false;
    shuffle := 1;
    tags := [ "W10Q2" ];

    qrec := rec( 
            title := title,
            type := "multi", 
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle,
            tags := tags );

	
    return qrec;


end;


distancesquared:= function( args... )
	local v,w,diff, A;

	v:=args[1]; w:=args[2]; A:=args[3];

	diff:=v-w;

	return (TransposedMat(diff)*A*diff)[1][1];

end;


sqrt:= function( args... )
	local d, sqrtd;

	d:=args[1];

	if d=0 then sqrtd:=0;fi;
	if d=1 then sqrtd:=1;fi;
	if d=4 then sqrtd:=2;fi;
	if d=9 then sqrtd:=3;fi;
	if d=16 then sqrtd:=4;fi;
	if d=25 then sqrtd:=5;fi;
	if d=36 then sqrtd:=6;fi;
	if d=49 then sqrtd:=7;fi;

return sqrtd;
end;




W10Q3 := function()

	local qrec, tags, qtext, subquestions, A, dim, numlist, v, myvec, distvecs, allvecs, dsquared, d, vecswithdist, i, j, max, numberofquestions, list1, list2;

	numlist:=[2,3]; dim:=Random(numlist);
	max:=1000;
	j:=max;
	numberofquestions:=4;
	
	while j = max do
	
		A:=0;
		while A=0 do
			A:=RandomPDMatrix(dim, [-4..5]);
			if A = NullMat(dim, dim) then A:=0; fi;
		od;

		myvec:=0;
		while myvec=0 do
			myvec:=RandomColumnVector(dim, [0..6]);
			if myvec = NullMat(dim, 1) then myvec:=0; fi;
		od;
		
		numlist:=[1,4,9,16,25,36];	

		distvecs:=[[],[],[],[],[],[]];
		allvecs:=[];
		vecswithdist:=[];
		j:=0;
		
		repeat
			v:=RandomColumnVector(dim,[0..5]); j:=j+1;
			dsquared:=distancesquared(v,myvec,A); 
			if dsquared in numlist then 
				d:=sqrt(dsquared);
				if Length(distvecs[d]) < 2 and not v in distvecs[d] then Add(distvecs[d],v); fi;
 				allvecs := Concatenation(distvecs);
			fi;
		until Length(allvecs) = numberofquestions or j = max;	

	od;



	for i in [1..numberofquestions] do
		for j in [1..6] do
			if allvecs[i] in distvecs[j] then Add(vecswithdist, [allvecs[i], j]);fi;
		od;
	od;

	list1 := List( vecswithdist, x->TransposedMat(x[1]) ); 

	list2 := List( vecswithdist, x->x[2] );
	list2:=Concatenation(list2,[1,2,3,4,5,6,9,16,25,36]);

	myvec:=TransposedMat(myvec);

	qtext:=Concatenation("Seja \\(A=", LaTeXObj( A ), "\\) e considere  o produto \n interno \\(\\langle -,-\\rangle_A\\).  Marque a distância entre o vetor dado \n e ", textvector(myvec),".");


	   tags := [ "W10Q3" ];

return MoodleMatchingQuestionByLists(list1, list2, "distance from v", qtext ,rec(tags:=tags));

end;










W10Q4 := function( )

	local qrec, subquestions, uu, vv, uv, values, maybe, title, qtext, answers, defgrade, 
            penalty, single, shuffle, tags;

	values:=[1..20];
	uu:=Random(values); vv:= Random(values); uv:=Random(values);

	if uv*uv <= uu*vv then maybe:=100; else maybe:=0; fi;	



    title := "Is IP given some values?";
    
    qtext :=  Concatenation("Considere o espaço vetorial \\(P_n\\) dos polinômios de grau no máximo \\(n\\)\n e suponha que temos um produto bilinear simétrico \\(\\langle -,-\\rangle\\) definido \n sobre \\(P_n\\).  Sejam \\(f,g\\in P_n\\) tais que $$\\langle f,g\\rangle =", String( uv ), ", \\langle f,f\\rangle =", String(uu), ", \\langle g,g\\rangle =", String(vv), ".$$  Qual das seguintes afirmações é correta?");
 



answers := [ ["\\(\\langle -,-\\rangle\\) é produto interno.", 
                                0], 
                 [ "\\(\\langle -,-\\rangle\\) não é produto interno.", 
                                100-maybe], 
                 [ "A partir dessas informações, não dá para afirmar \n se \\(\\langle -,-\\rangle\\) é produto interno ou não.", 
                                maybe]];




    defgrade := 1;
    penalty := "0.1";
    single := true;
    shuffle := 1;
    tags := [ "W10Q4" ];

    qrec := rec( 
            title := title,
            type := "multi", 
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle,
            tags := tags );

	
    return  qrec;


end;







W14Q1 := function( )

	local qrec, subquestions, title, qtext, defgrade, 
            penalty, single, shuffle, answers, tags, P, t, okdets, e1, e2, f, eigs, eigs1, eigs2, eigs3, eigs4, D, A, D1, D2, D3, s, g, diagsA;

	s:=0;
	repeat
	
		okdets:=[1,-1];
		t:=0;
		repeat
			P:=RandomMatrix(3,[-1..2]);
			if Determinant(P) in okdets and not IsDiagonalMat(P) then t:=1; fi;
		until t=1;
	
		eigs:=Set([-2,-1,1,2,3,4]);
		e1:=Random(eigs);
		t:=0;
		repeat
			e2:=Random(eigs); if not e1=e2 then t:=1; fi;
		until t=1;

		t:=0;
		repeat
			f:=Random(eigs); if not e1=f and not e2=f then t:=1; fi;
		until t=1;

		g:=Random(Set([e1,e2]));

		eigs:=Shuffle([e1,e1,e2]);
		eigs1:=Shuffle([e1,e2,e2]);
		eigs2:=Shuffle([g,f,f]);
		eigs3:=Shuffle([f,g,g]);


		D:=DiagonalMat(eigs);
		A:=P*D*P^-1;
		D1:=DiagonalMat(eigs1);
		D2:=DiagonalMat(eigs2);
		D3:=DiagonalMat(eigs3);

		diagsA:=[A[1][1], A[2][2], A[3][3]]; 
		Sort(diagsA);
		Sort(eigs);

		if not eigs = diagsA then s:=1; fi;
	
	until s=1;



  title := "Qual semelhante?";
    
    qtext :=  Concatenation("Qual das seguintes matrizes é semelhante à matriz $$", LaTeXObj(A), "?$$");
 
	

answers := [ [Concatenation("\\(",LaTeXObj(D),"\\)"), 
                                100], 
                 [ Concatenation("\\(",LaTeXObj(D1),"\\)"), 
                                0], 
                 [ Concatenation("\\(",LaTeXObj(D2),"\\)"), 
                                0], 
                 [ Concatenation("\\(",LaTeXObj(D3),"\\)"), 
                                0]];




    defgrade := 1;
    penalty := "0.1";
    single := true;
    shuffle := 1;
    tags := [ "W14Q1" ];

    qrec := rec( 
            title := title,
            type := "multi", 
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle,
            tags := tags );

	
    return  qrec;

end;



nicexyz := function( args...)
	local v, i, strings;
	v:=args[1];

	strings:=[];

		if v[1] > 0 and not v[1]=1 then strings[1]:=Concatenation(String(v[1]), "x"); fi;
		if v[1] = 1 then strings[1]:="x"; fi;
		if v[1] = 0 then strings[1]:=""; fi;
		if v[1] < 0 and not v[1]=-1 then strings[1]:=Concatenation(String(v[1]), "x"); fi;
		if v[1] = -1 then strings[1]:="-x"; fi;

	if v[1] = 0 then
	
		if v[2] > 0 and not v[2]=1 then strings[2]:=Concatenation(String(v[2]), "y"); fi;
		if v[2] = 1 then strings[2]:="y"; fi;
		if v[2] = 0 then strings[2]:=""; fi;
		if v[2] < 0 and not v[2]=-1 then strings[2]:=Concatenation(String(v[2]), "y"); fi;
		if v[2] = -1 then strings[2]:="-y"; fi;
	else

		if v[2] > 0 and not v[2]=1 then strings[2]:=Concatenation("+", String(v[2]), "y"); fi;
		if v[2] = 1 then strings[2]:="+y"; fi;
		if v[2] = 0 then strings[2]:=""; fi;
		if v[2] < 0 and not v[2]=-1 then strings[2]:=Concatenation(String(v[2]), "y"); fi;
		if v[2] = -1 then strings[2]:="-y"; fi;
	fi;

	if v[1] = 0 and v[2] = 0 then
	
		if v[3] > 0 and not v[3]=1 then strings[3]:=Concatenation(String(v[3]), "z"); fi;
		if v[3] = 1 then strings[3]:="z"; fi;
		if v[3] = 0 then strings[3]:=""; fi;
		if v[3] < 0 and not v[3]=-1 then strings[3]:=Concatenation(String(v[3]), "z"); fi;
		if v[3] = -1 then strings[3]:="-z"; fi;

	else
		if v[3] > 0 and not v[3]=1 then strings[3]:=Concatenation("+", String(v[3]), "z"); fi;
		if v[3] = 1 then strings[3]:="+z"; fi;
		if v[3] = 0 then strings[3]:=""; fi;
		if v[3] < 0 and not v[3]=-1 then strings[3]:=Concatenation(String(v[3]), "z"); fi;
		if v[3] = -1 then strings[3]:="-z"; fi;
	fi;

	if v = [0,0,0] then strings[3]:="0";fi;

	return Concatenation(strings[1], strings[2], strings[3]);
end;




W14Q2 := function( )

	local qrec, subquestions, title, qtext, defgrade, 
            penalty, single, shuffle, answers, tags, P, t, okdets, e1, e2, eigs, eigs1, eigs2, eigs3, eigs4, D, A, s, diagsA, u, v, w, i, vec, options;

	s:=0;
	repeat
	
		okdets:=[1,-1];
		t:=0;
		repeat
			P:=RandomMatrix(3,[-2..3]);
			if Determinant(P) in okdets and not IsDiagonalMat(P) then t:=1; fi;
		until t=1;
	
		eigs:=Set([-2,-1,1,2,3,4]);
		e1:=Random(eigs);
		t:=0;
		repeat
			e2:=Random(eigs); if not e1=e2 then t:=1; fi;
		until t=1;


		eigs:=[e1,e1,e2];


		D:=DiagonalMat(eigs);
		A:=P*D*P^-1;
	
		diagsA:=[A[1][1], A[2][2], A[3][3]]; 
		Sort(diagsA);
		Sort(eigs);

		if not eigs = diagsA then s:=1; fi;
	
	until s=1;

	u:= textvector([[P[1][1], P[2][1], P[3][1]]]);
	v:= textvector([[P[1][2], P[2][2], P[3][2]]]);
	w:= [P[1][3], P[2][3], P[3][3]];

	
	options:=[];
	for i in [1..3] do


		repeat
			t:=0;
			vec:=TransposedMat(RandomColumnVector(3,[-2..3]));;
			if vec[1][1] = w[1] and not vec[1][2] = w[2] then t:=1; fi;
		until t=1;
		options[i]:=vec;
  	od;

	w:= textvector([[P[1][3], P[2][3], P[3][3]]]);
		

	title := "Diagonalize T";
    
 
 qtext :=  Concatenation("Considere a transformação linear \\(T : \\mathbb{R}^3 \\to \\mathbb{R}^3\\) \n dada por  $$T(x,y,z) = (", String(nicexyz([A[1][1],A[1][2],A[1][3]])), "\\,,\\, ", String(nicexyz([A[2][1],A[2][2],A[2][3]])), "\\,,\\, ", String(nicexyz([A[3][1],A[3][2],A[3][3]])), ").$$ \n Encontre o vetor \\( \\underline{w}\\) tal que \\(T\\) com respeito \n à base \\(\\underline{u}, \\underline{v}, \\underline{w}\\) é diagonal, \n onde $$\\underline{u} = ", u ,"\\,,\\, \\underline{v} = ", v ,".$$");
 	

answers := [ [Concatenation("\\(",w,"\\)"), 
                                100], 
               [ Concatenation("\\(",textvector(options[1]),"\\)"), 
                                0], 
                 [ Concatenation("\\(",textvector(options[2]),"\\)"), 
                                0], 
                 [ Concatenation("\\(",textvector(options[3]),"\\)"), 
                                0]];





    defgrade := 1;
    penalty := "0.1";
    single := true;
    shuffle := 1;
    tags := [ "W14Q2" ];

    qrec := rec( 
            title := title,
            type := "multi", 
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle,
            tags := tags );

	
    return  qrec;

end;












W14Q3 := function( )

	local qrec, subquestions, numbercorrect, marks, rightnorm, title, qtext, answers, defgrade, penalty, single, shuffle, tags, smallA, lastentry, A, d, rest, i, j, k, options, 
pos, notpos, questionlist, X, range;

	
	
	range:=[-4..8];
	pos:=[1..9];
	notpos:=[-9..-1];
	options:=[];

	# following makes a PD matrix

	repeat	
		smallA:=RandomPDMatrix(2, range);
	until not smallA[1][2] = 0 and not smallA[2][1] = 0;
	
	lastentry:=Random(pos);

	d:=Random([1..3]);
	rest:=[1,2,3];
	RemoveSet(rest, d);
	
	A:=NullMat(3,3);
	A[d,d]:=lastentry;
	for i in [1..2] do
		for j in [1..2] do
			A[rest[i]][rest[j]]:=smallA[i,j];
		od;
	od;

	questionlist:=[];
	questionlist[1]:=[A,1];

	# following makes 2 not PD matrices with positive loose eigenvalue

	for k in [1..2] do

		repeat	
			smallA:=RandomSymmetricMatrix(2, range);
		until not smallA[1][2] = 0 and not smallA[2][1] = 0 and not IsPositiveDefinite(smallA);

		lastentry:=Random(pos);

		d:=Random([1..3]);
		rest:=[1,2,3];
		RemoveSet(rest, d);
	
		A:=NullMat(3,3);
		A[d,d]:=lastentry;
		for i in [1..2] do
			for j in [1..2] do
				A[rest[i]][rest[j]]:=smallA[i,j];
			od;
		od;

		options[k]:=[A,2];
	od;

	# following makes a not PD matrix with negative loose eigenvalue but other two eigenvalues positive

	repeat	
		smallA:=RandomPDMatrix(2, range);
	until not smallA[1][2] = 0 and not smallA[2][1] = 0;
	
	lastentry:=Random(notpos);

	d:=Random([1..3]);
	rest:=[1,2,3];
	RemoveSet(rest, d);
	
	A:=NullMat(3,3);
	A[d,d]:=lastentry;
	for i in [1..2] do
		for j in [1..2] do
			A[rest[i]][rest[j]]:=smallA[i,j];
		od;
	od;

	options[3]:=[A,2];


	# following makes 2 more PD matrices, which could be diagonal

	for k in [4..5] do

			smallA:=RandomPDMatrix(2, range);

		lastentry:=Random(pos);

		d:=Random([1..3]);
		rest:=[1,2,3];
		RemoveSet(rest, d);
	
		A:=NullMat(3,3);
		A[d,d]:=lastentry;
		for i in [1..2] do
			for j in [1..2] do
				A[rest[i]][rest[j]]:=smallA[i,j];
			od;
		od;

		options[k]:=[A,1];
	od;

	options:=Set(options);

  	  numbercorrect:=1;


	    X := Random(options);
	    if X[2]=1 then 
	        numbercorrect := numbercorrect+1;
	        Add( questionlist, X);
	    else 
	        Add( questionlist, X); 
	    fi;
	    RemoveSet(options, X);




	    X := Random(options);
	    if X[2]=1 then 
	        numbercorrect := numbercorrect+1;
	        Add( questionlist, X);
	    else 
	        Add( questionlist, X); 
	    fi;
	    RemoveSet(options, X);



	    X := Random(options);
	    if X[2]=1 then 
	        numbercorrect := numbercorrect+1;
	        Add( questionlist, X);
	    else 
	        Add( questionlist, X); 
	    fi;
	    RemoveSet(options, X);



 	   marks:=[0,0];
	    if numbercorrect = 1 then marks[1]:="100"; marks[2]:="-33.333";fi;
 	   if numbercorrect = 2 then marks[1]:="50"; marks[2]:="-50";fi;
 	   if numbercorrect = 3 then marks[1]:="33.333"; marks[2]:="-100";fi;



  title := "find IP matrices";
    
    qtext :=  "Marque as matrizes \\( A \\) com respeito a quais \n a forma bilinear \\(\\langle -,-\\rangle_A\\) é um \n produto interno.";
 

answers := [ [ Concatenation("\\(",String(LaTeXObj(questionlist[1][1])),"\\)"), 
                                marks[questionlist[1][2]]], 
                 [ Concatenation("\\(",String(LaTeXObj(questionlist[2][1])),"\\)"), 
                                marks[questionlist[2][2]]], 
                 [ Concatenation("\\(",String(LaTeXObj(questionlist[3][1])),"\\)"), 
                                marks[questionlist[3][2]]],
                 [ Concatenation("\\(",String(LaTeXObj(questionlist[4][1])),"\\)"), 
                                marks[questionlist[4][2]] ]];




    defgrade := 1;
    penalty := "0.1";
    single := false;
    shuffle := 1;
    tags := [ "W14Q3" ];

    qrec := rec( 
            title := title,
            type := "multi", 
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle,
            tags := tags );



 	
	
    return qrec;


end;


skewmatrix:= function(args...)
	local n, range, A, i, j, val;

	n:=args[1]; range:=args[2];

	A:=NullMat(n,n);

	for i in [1..n-1] do
		for j in [i+1..n] do
			val:=Random(range);
			A[i][j]:=val; A[j][i]:=-val;
		od;
	od;
	return A;
end;


RandomOrthoMatrix:= function (args...)
	local A, B, I, n, range;
	n:=args[1];
	range:=args[2];
	A:=skewmatrix(n,range);
	I:=IdentityMat(n);

	B:=(A-I)^-1*(A+I);

return B;
end;








W14Q4 := function( )

	local qrec, subquestions, title, qtext, defgrade, 
            penalty, single, shuffle, answers, tags, omat, denom, s,i,j,k, omatint, u,v,w, questionmat,a,b,c,P, options, vec, range,t;

	repeat

		omat := RandomOrthoMatrix(3,[-3..3]);
		denom:=DenominatorRat(omat[1][1]);

		s:=0;
		for i in [1..3] do
			for j in [1..3] do
				if denom = 1 then s:=1;fi;
				if not DenominatorRat(omat[i][j]) = denom then s:=1; fi;
			od;
		od;
	until s=0;
	
	omatint:=denom*omat;

	u:=omatint[1];
	v:=omatint[2];
	w:=omatint[3];

	P:=PolynomialRing( Rationals, ["a", "b", "c"] );
	questionmat:=One(P) * MutableCopyMat(TransposedMat(omatint));
	

	questionmat[1][3]:=P.1; questionmat[2][3]:=P.2; questionmat[3][3]:=P.3;

	options:=[];

	for i in [1..3] do


	range:=[Minimum(w)-1..Maximum(w)+1];
		repeat
			t:=0;
			vec:=TransposedMat(RandomColumnVector(3,range));;
			if vec[1][1] = w[1] and not vec[1][2] = w[2] then t:=1; fi;
		until t=1;
		options[i]:=vec;
  	od;




	title := "complete orthogonal matrix";
    
 
# qtext :=  Concatenation("Qual dos seguintes vetores \n \\((a,b,c)\\) se torna a matriz \n \\(P\\) ortogonal, onde \n $$P = \\frac{1}{",String(denom),"}",String(LaTeXObj(questionmat)),"?$$");

	qtext :=  Concatenation("Escolhe os valores \n \\(a,b,c\\) de tal forma que a inversa da matriz \n \\(P\\) seja igual a sua transposta, onde \n $$P = \\frac{1}{",String(denom),"}",String(LaTeXObj(questionmat)),".$$");
  	

answers := [ [Concatenation("\\((a,b,c) = ",textvector([w]),"\\)"), 
                                100], 
               [ Concatenation("\\((a,b,c) = ",textvector(options[1]),"\\)"), 
                                0], 
                 [Concatenation("\\((a,b,c) = ",textvector(options[2]),"\\)"), 
                                0], 
                 [ Concatenation("\\((a,b,c) = ",textvector(options[3]),"\\)"), 
                                0]];





    defgrade := 1;
    penalty := "0.1";
    single := true;
    shuffle := 1;
    tags := [ "W14Q4" ];

    qrec := rec( 
            title := title,
            type := "multi", 
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle,
            tags := tags );


	
    return qrec;

end;


makequestionlist := function (args...) 
	local n, Q, i, L;

	n:=args[1];
	Q:=args[2];
	L:=[];

	for i in [1..n] do
		Add(L, Q());
	od;
return L;
end;

