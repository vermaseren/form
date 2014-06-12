CFunction evalSacc;
Symbols evalSj,evalSm,evalSn;
*
#procedure evalS(S,R,par)
*
*  Procedure for the evaluation of harmonic sums with postive integer
*  argument. It handles both the S-sum and the Z-sum definitions.
*  Call with
*       #call evalS(SumName,RName,S)   for S-sums
*       #call evalS(SumName,RName,Z)   for Z-sums
*  The SumName and RName should be declared as functions in the
*  calling program (either commuting or non-commuting).
*
id  `S'(`R'(evalSm?,?a),evalSn?pos_) =
                    evalSacc(`S'(`R'(evalSm,?a),evalSn));
id  `S'(`R'(evalSm?,?a),0) = 0;
repeat;
  Argument evalSacc;
    #switch `par'
    #case S
      id  `S'(`R'(evalSm?pos_,?a),evalSn?pos_) =
             sum_(evalSj,1,evalSn,`S'(`R'(?a),evalSj)/evalSj^evalSm);
      id  `S'(`R'(evalSm?neg_,?a),evalSn?pos_) =
             sum_(evalSj,1,evalSn,`S'(`R'(?a),evalSj)*
                                           sign_(evalSj)*evalSj^evalSm);
      id  `S'(`R',evalSn?) = 1;
    #break
    #case Z
      id  `S'(`R'(evalSm?pos_,?a),evalSn?pos_) =
           sum_(evalSj,1,evalSn,`S'(`R'(?a),evalSj-1)/evalSj^evalSm);
      id  `S'(`R'(evalSm?neg_,?a),evalSn?pos_) =
           sum_(evalSj,1,evalSn,`S'(`R'(?a),evalSj-1)*
                                           sign_(evalSj)*evalSj^evalSm);
      id  `S'(`R',evalSn?) = 1;
      id  `S'(`R'(?a),0) = 0;
    #break
    #default
      #write "Unrecognized last argument in procedure evalS: `par'"
      #write "Argument should be either S or Z"
      #terminate
    #break
    #endswitch
  EndArgument;
endrepeat;
id  evalSacc(evalSn?) = evalSn;
#endprocedure

