

type ( 't1, 't0 ) nonterm =
    {
        _Const2: ( 't1 ) Camlburg.nt;
        _Add1: ( 't0 ) Camlburg.nt;
        str: ( ( string  ) ) Camlburg.nt;
        number: ( ( int     ) ) Camlburg.nt
    }

let rec
inf =
    {number = (Camlburg.infinity)
    ;str = (Camlburg.infinity)
    ;_Add1 = (Camlburg.infinity)
    ;_Const2 = (Camlburg.infinity)
    }


let rec
update_number =
    fun nt x ->
        if nt.Camlburg.cost >= x.number.Camlburg.cost then
            x
        else
            (fun x ->
                (update_str
                    {Camlburg.cost = (nt.Camlburg.cost + 1)
                    ;Camlburg.action =
                        (fun () ->
                            let n = x.number.Camlburg.action ()
                            in
                                
# 24 "sample.mlb"
                                ( string_of_int n      )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with number = (nt) }
and update_str =
    fun nt x ->
        if nt.Camlburg.cost >= x.str.Camlburg.cost then
            x
        else
            (fun x ->
                (update_number
                    {Camlburg.cost = (nt.Camlburg.cost + 1)
                    ;Camlburg.action =
                        (fun () ->
                            let str = x.str.Camlburg.action ()
                            in
                                
# 25 "sample.mlb"
                                ( int_of_string str    )
                                
# 000 "/dev/stdout"
)
                    })
                    x)
                { x with str = (nt) }
and update__Add1 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add1.Camlburg.cost then
            x
        else
            { x with _Add1 = (nt) }
and update__Const2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Const2.Camlburg.cost then
            x
        else
            { x with _Const2 = (nt) }


let rec
conSub =
    fun arg1 arg2 ->
        (update_number
            {Camlburg.cost =
                (arg1.number.Camlburg.cost + arg2.number.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1.number.Camlburg.action ()
                    and m = arg2.number.Camlburg.action ()
                    in
                        
# 12 "sample.mlb"
                        ( n-m )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conStr =
    fun arg1 ->
        (update_str
            {Camlburg.cost =
                (let x = arg1
                in
                    
# 19 "sample.mlb"
                    ( String.length x )
                    
# 000 "/dev/stdout"
)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1
                    in
                        
# 19 "sample.mlb"
                        ( x )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update_number
            {Camlburg.cost =
                (arg1.number.Camlburg.cost + arg2.number.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1.number.Camlburg.action ()
                    and m = arg2.number.Camlburg.action ()
                    in
                        
# 13 "sample.mlb"
                        ( n*m )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conDiv =
    fun arg1 arg2 ->
        (update_number
            {Camlburg.cost =
                (arg1.number.Camlburg.cost + arg2.number.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1.number.Camlburg.action ()
                    and m = arg2.number.Camlburg.action ()
                    in
                        
# 14 "sample.mlb"
                        ( if m = 0 then assert false else n/m )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conConst =
    fun arg1 ->
        (update__Const2
            {Camlburg.cost = ((Camlburg.matches 0) arg1)
            ;Camlburg.action = (fun () -> ())
            })
            ((update_number
                (Camlburg.choice
                    [{Camlburg.cost = (1)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1
                            in
                                
# 15 "sample.mlb"
                                ( x )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost = ((Camlburg.matches 0) arg1)
                    ;Camlburg.action =
                        (fun () ->
                            
# 16 "sample.mlb"
                            ( 0 )
                            
# 000 "/dev/stdout"
)
                    }]))
                inf)
and conCons =
    fun arg1 arg2 ->
        (update_str
            {Camlburg.cost = (2)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1
                    and y = arg2
                    in
                        
# 20 "sample.mlb"
                        ( x ^ y )
                        
# 000 "/dev/stdout"
)
            })
            inf
and conAdd =
    fun arg1 arg2 ->
        (update__Add1
            {Camlburg.cost =
                (arg1.number.Camlburg.cost + arg2.number.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let y = arg1.number.Camlburg.action ()
                    and z = arg2.number.Camlburg.action ()
                    in
                        (y ,z))
            })
            ((update_number
                (Camlburg.choice
                    [{Camlburg.cost =
                        (2 + arg1.number.Camlburg.cost
                        +
                        arg2.number.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.number.Camlburg.action ()
                            and y = arg2.number.Camlburg.action ()
                            in
                                
# 8 "sample.mlb"
                                ( x + y )
                                
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (2 + arg1.number.Camlburg.cost
                        +
                        arg2._Add1.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.number.Camlburg.action ()
                            and _v1 = arg2._Add1.Camlburg.action ()
                            in
                                let (y, z) = _v1
                                in
                                    
# 9 "sample.mlb"
                                    ( x + y + z)
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (1 + arg1.number.Camlburg.cost
                        +
                        arg2._Const2.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.number.Camlburg.action ()
                            and _v1 = arg2._Const2.Camlburg.action ()
                            in
                                let () = _v1
                                in
                                    
# 10 "sample.mlb"
                                    ( x     )
                                    
# 000 "/dev/stdout"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Const2.Camlburg.cost
                        +
                        arg2._Const2.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Const2.Camlburg.action ()
                            and _v2 = arg2._Const2.Camlburg.action ()
                            in
                                let () = _v2
                                in
                                    let () = _v1
                                    in
                                        
# 11 "sample.mlb"
                                        ( 0     )
                                        
# 000 "/dev/stdout"
)
                    }]))
                inf)


