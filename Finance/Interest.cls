    Function Interest (n as Integer,pv as Double, pmt as Double, fv as Double, cy as Integer, py as Integer,Optional s as Integer = 0) as Double
        Dim i as Double, g as Integer
        i = 0.001
        
        'newtons method of derivation
        Dim fDerive as Double, sDerive as Double, precision as Integer
        precision = 30 'loop 30 times to achieve better precision
        For g = 0 To precision
            'underived equation
            fDerive = pv + pmt * ((1 + i * s) * ((1 + i) ^ n - 1) / (i * (1 + i) ^ n)) + fv * 1 / (1 + i) ^ n
            'first derivation
            sDerive = (pmt / i) * ((-(1 + i * s) * (1 - (1 + i) ^ (-n)) / i) + (1 + i * s) * (n * (1 + i) ^ (-n - 1)) + s * (1 - (1 + i) ^ (-n))) - n * fv * (1 + i) ^ (-n - 1)
            'uses newton method to find answer
            i = (i - fDerive / sDerive)
        Next g
       
        Interest =  ((1 + i / 100) ^ (py / cy) - 1) * cy * 100 * 100    
    End Function
