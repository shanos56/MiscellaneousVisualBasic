    Function FutureValue (n as Integer, i as Double, pv as Double, pmt as Double, cy as Integer, py as Integer,Optional s as Integer = 0) as Double
        Dim vi As Double, EFF As Double, al As Double, be As Double
        EFF = ((1 + (i / (100 * cy))) ^ (cy / py) - 1) * 100
    
        vi = EFF / 100 'returns new interest rate in decimal form
        's = 1 beginning of term
        's = 0 end of term
        If s <> 1 And s <> 0 Then
            s = 0
        End If       
        
        if i <> 0 then
            dim al = ((1 + vi * s) * ((1 + vi) ^ n - 1)) / (vi * ((1 + vi) ^ n)) 'finds alpha
            dim be = (1 / ((1 + vi) ^ (n))) 'finds beta
            FutureValue = -(pmt * al + pv) / be  
        else
            FutureValue = -(pmt * n + pv) 
        End if
    End Function
