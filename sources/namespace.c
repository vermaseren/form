/*
    Namespaces

    Syntax:
        #namespace name


        #endnamespace

    Inside the namespace all local variables must be declared.
    Internally they are represented by an attached _name.
    This gives a problem when we have
        #procedure proc(a)
        #namespace proc
            Symbol x,y;
            .......
            id  x = `a';
        #endnamespace
        #endprocedure
    There will be the risk that `a' is a global variable y and if we 
    start with substituting the preprocessor variables we cannot avoid 
    `a' ending up as y_proc which is probably not what we want.
    Potential solution:
        prepend all local variables with _.
        The alternative to prepend all global variables with _
        cannot work when `a' evaluates into (a+b).

    Potential problems

    1: what to do with local variables that remain inside expressions after
    the first .sort that is encountered when the namespace is closed?
    Should be an error. The .sort should remove the variables from the
    variable administration.
    2: in the regular expressions the above can be checked. What about
    $-variables? stored expressions? hidden expressions?
    3: how about local $-variable names?
    4: We may also need local expression names. Also those should be gone
    after the first .sort after the #endnamespace.

    Next problem: two successive namespaces inside the same module.
    This can give undesirable interferences.
    One potential solution could be to have #endnamespace generate an
    automatic .sort, but then we cannot use constructs like
        if (.....);
            #call proc1
        elseif (.....);
            #call proc2
        endif;
    and having this followed by
        #call proc3
    could have the above interferences if each procedure has its own
    namespace.
    As of yet no good solution.

    The only rigorous solution seems to be that one should demand that
    already in the expression tree, each term that leaves the namespace
    does not contain any variable that belongs to that namespace. Hence
    one cannot wait for terms to cancel! Is this desirable?
    (this would be equivalent to
        #namespace name
            Symbol _x,_y;
            CFunction _f;
            .......
            .......
            if ( occurs(_x,_y,_f) )
                    exit "local variables of namespace in output";
        #endnamespace
    and local dollar variables will cease to exist.)
    Even with this there can still be problems, but they may be easier 
    to solve.
*/
