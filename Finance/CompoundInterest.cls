
'Replication of Casio TVM Solver Compounding Interest (Coded in Visual Basic)
Function TVMNewtonMethod(n As Variant, i As Double, pv As Variant, pmt As Variant, fv As Variant, py As Double, cy As Double, s As Integer) As Double
'newtons method of derivation
Dim fDerive As Double, sDerive As Double, precision As Integer
precision = 30 'loop 30 times to achieve better precision
        For g = 0 To precision
'underived equation
fDerive = pv + pmt * ((1 + i * s) * ((1 + i) ^ n - 1) / (i * (1 + i) ^ n)) + fv * 1 / (1 + i) ^ n
        'first derivation
        sDerive = (pmt / i) * ((-(1 + i * s) * (1 - (1 + i) ^ (-n)) / i) + (1 + i * s) * (n * (1 + i) ^ (-n - 1)) + s * (1 - (1 + i) ^ (-n))) - n * fv * (1 + i) ^ (-n - 1)
'uses newton method to find answer
i = (i - fDerive / sDerive)
        Next g
        TVMNewtonMethod = i
End Function    


's (Term) is optional
's default is 0 (End of Term)
Function COMPOUND(n As Variant, i As Variant, pv As Variant, pmt As Variant, fv As Variant, py As Double, cy As Double, Optional s As Integer = 0) As Variant
        Dim vi As Double, EFF As Double, al As Double, be As Double
        If i <> "?" Then
            EFF = ((1 + (i / (100 * cy))) ^ (cy / py) - 1) * 100
        Else
           EFF = 1
        End If        
        vi = EFF / 100 'returns new interest rate in decimal form
's = 1 beginning of term
's = 0 end of term
If s <> 1 And s <> 0 Then
    s = 0
End If               
'if n is unknown then this part of equation is void
'if i is unknown is also void
'different equation used if i = 0
If n <> "?" And i <> "?" And i <> 0 Then
            al = ((1 + vi * s) * ((1 + vi) ^ n - 1)) / (vi * ((1 + vi) ^ n)) 'finds alpha
            be = (1 / ((1 + vi) ^ (n))) 'finds beta
        End If
        If i <> "?" And i <> 0 Then
    'finds if any variable is equal to ?
    Select Case "?"
                Case n
                    COMPOUND = Log(((1 + vi * s) * pmt - fv * vi) / ((1 + vi * s) * pmt + pv * vi)) / Log(1 + vi)                    
                Case pv
                    COMPOUND = -(pmt * al + fv * be)                    
                Case fv
                    COMPOUND = -(pmt * al + pv) / be                
                Case pmt
                    COMPOUND = -(pv + fv * be) / al            
            End Select
        ElseIf i = 0 Then 'when interest is 0 requires different calculation
            Select Case "?"            
                Case n
                    COMPOUND = -(pv + fv) / pmt                    
                Case pv
                    COMPOUND = -(pmt * n + fv)                    
                Case fv
                    COMPOUND = -(pmt * n + pv)                
                Case pmt
                    COMPOUND = -(pv + fv) / n            
            End Select        
        ElseIf i = "?" Then 'Uses newtons method to find interest (it won't be exact)       
            vi = TVMNewtonMethod(n, 0.001, pv, pmt, fv, cy, py, s)
            COMPOUND = ((1 + vi / 100) ^ (py / cy) - 1) * cy * 100 * 100    
        End If        
End Function
