""" PM Lab Assignment 
    Design a New Language          " R o S h "
    
    Submitted by -
                   ADARSH KUMAR JAIN
                   ROHIT DEEGWAL
"""

import time


def isdatatype(s):
    if( s=="int" or s=="float" or s=="string" ):
        return True
    else:
        return False

def isoperator(s):
    if( s=="+" or s=="-" or s=="*" or s=="/" or s=="%" ):
        return True;
    else:
        return False;

def isassgop(s):
    if( s=="+=" or s=="-=" or s=="*=" or s=="/=" or s=="%=" or s=="=" ):
        return True
    else:
        return False
    
def isrelationalop(s):
    if( s==">" or s==">=" or s=="<" or s=="<=" or s=="==" or s=="!=" ):
        return True
    else:
        return False

def eval_expr(j,p,g,datatype,value):
    global error
    var = p[j-1]
    expr=""
    j=j+1
    while( j<len(p) and p[j] != ',' and p[j] != ';' ):
        if( p[j].isidentifier()  and p[j-1] != "\"" ):
            if( datatype.get(p[j]) == None):
                fp.write(g+": '"+p[j]+"' undeclared\n")
                print(g+": '"+p[j]+"' undeclared")
                expr=expr+p[j]
                error = error+1
            
            else:
                if( value.get(p[j]) == None ):
                    fp.write(g+": '"+p[j]+"' uninitialized\n")
                    print(g+": '"+p[j]+"' uninitialized")
                    expr=expr+p[j]
                    error = error+1
                elif( value.get(p[j]) != None ):
                    expr=expr+str(value.get(p[j]))
        else:
            expr=expr+p[j]
        j=j+1
    
    try:
        result = eval(expr)
    except:
        pass            
    else:  
        if( datatype.get(var)=="int" and isinstance(result,float) ):
            result = int(result)
            value[var] = result
        
        elif( datatype.get(var)=="int" and isinstance(result,str) ):
            if( len(result) == 1 ):
                result = ord(result)
                value[var] = result
            else:
                fp.write(g+": string '"+result+"' can not be convert into int\n")
                print(g+": string '"+result+"' can not be convert into int")
                error = error+1
        
        elif( datatype.get(var)=="float" and isinstance(result,int) ):
            result = float(result)
            value[var] = result
        
        elif( datatype.get(var)=="float" and isinstance(result,str) ):
            fp.write(g+": incompatible types when initializing type 'float'\n")
            print(g+": incompatible types when initializing type 'float'")
            error = error +1
        
        elif( datatype.get(var)=="string" and isinstance(result,int) ):
            result = chr(result)
            value[var] = result
        
        elif( datatype.get(var)=="string" and isinstance(result,float) ):
            fp.write(g+": incompatible types when initializing type 'string'\n")
            print(g+": incompatible types when initializing type 'string'")
            error = error +1
        else:
            value[var] = result
    return j


def check_expr(a,index,datatype,value):
    p = a.split()
    g = str(index)
    bkt = 0
    global bracket
    global error
    
    """if( p[0] == "{" ):
        bracket = bracket+1
    if( p[-1] == "}" and p[-2] != ";" ):
        fp.write(g+": Semicolon is missing\n")
        print(g+": Semicolon is missing")
        error = error + 1
        bracket = bracket-1
    elif( p[-1] != ";" ):
        fp.write(g+": Semicolon is missing\n")
        print(g+": Semicolon is missing")
        error = error + 1"""

    # if equal is not in list, then ValueError
    try:
        i = p.index("=")
        for j in range(i-1):
            if( isoperator(p[j]) ):
                fp.write(g+": can not use a airthmatic operator at the left side of equal operator\n")
                print(g+": can not use a airthmatic operator at the left side of equal operator")
                error = error + 1
    except(ValueError):
        pass
    
    for i in range(len(p)):
        try:
            if( p[i] == "=" and isoperator(p[i-1]) ):
                z = p.pop(i)
                p[i-1] = p[i-1]+str(z)
            
            if( p[i] == "=" and p[i+1] == "=" ):
                z = p.pop(i+1)
                p[i] = p[i]+str(z)                
        
            if( p[i] == "+" and p[i+1] == "+" ):
                z = p.pop(i+1)
                p[i] = p[i]+str(z)
            
            if( p[i] == "-" and p[i+1] == "-" ):
                z = p.pop(i+1)
                p[i] = p[i]+str(z)
                
            if( p[i] == "/" and p[i+1] == "/" ):
                z = p.pop(i+1)
                p[i] = p[i]+str(z)            
        except(IndexError):
            pass
    
    try:
        x = p.index("==")
        for j in range(x+1,len(p)-1):
            if( p[j] == "=" ):
                fp.write(g+": can't assign to comparison\n")
                print(g+": can't assign to comparison")
                error = error + 1
    except:
        pass
    
    
    
    for i in range(len(p)):
        if( p[i] == "(" ):
            bkt = bkt+1       
            if( not isoperator(p[i-1]) and (not isassgop(p[i-1]) ) and p[i-1] != "=" and p[i-1] != '('):
                fp.write(g+": There should be an operator before parenthisis\n")
                print(g+": There should be an operator before parenthisis")
                error = error+1
        
        if( p[i] == ")" ):
            bkt = bkt-1
            if( i == len(p) -1 ):
                continue
            elif( not isoperator(p[i+1]) and p[i+1] != ";" and p[i+1] != ")" and p[i+1] != "," ):
                fp.write(g+": There should be an operator after parenthisis\n")
                print(g+": There should be an operator after parenthisis")
                error = error+1
        
        if( p[i] == "=" and isoperator(p[i+1]) ):
            fp.write(g+": there can not "+str(p[i+1])+" operator after equal sign\n")
            print(g+": there can not "+str(p[i+1])+" operator after equal sign")
            error = error+1
            
        if( p[i] == "++" and p[i-1].isdigit() ):
            fp.write(g+": Left value required as increment operand before ++ operator\n")
            print(g+": Left value required as increment operand before ++ operator")
            error = error+1
            
        if( p[i] == "--" and p[i-1].isdigit() ):
            fp.write(g+": Left value required as increment operand before -- operator\n")
            print(g+": Left value required as increment operand before -- operator")
            error = error+1
        
        if( isassgop(p[i]) and p[i-1].isdigit()):
            fp.write(g+": left value required as left operand of assignent\n")
            print(g+": left value required as left operand of assignent")
            error = error+1
    
    if( bkt != 0 ):
            fp.write(g+": unmatched brackets in expressions\n")
            print(g+": unmatched brackets in expressions")
            error = error+1
    
    j=0
    while( j<len(p) ):
    
        if( p[j] == "," ):
            if( not p[j+1].isidentifier() ):
                fp.write(g+": "+p[j+1]+" should be a valid identifier.\n")
                print(g+": "+p[j+1]+" should be a valid identifier.")
                error = error +1
        
        if( p[j] == "=" ):
            
            var = p[j-1]
            if( datatype.get(var) == None ):
                fp.write(g+": '"+var+"' undeclared\n")
                print(g+": '"+var+"' undeclared")
                error = error+1
                j=j+1
                continue
            
            j = eval_expr(j,p,g,datatype,value)

            
        j=j+1
        
    if( p[-1] != ";" ):
        fp.write(g+": Semicolon is missing\n")
        print(g+": Semicolon is missing")
        error = error + 1    
    #print(value)
    #print(datatype)


def check_declaration(a,index,datatype,value):
    p = a.split()
    g = str(index)
    result=0
    global bracket
    global error
    expr=""
    
    if( p[0] == "{" ):
        bracket = bracket+1
    
    if( not p[1].isidentifier() ):
        fp.write(g+": "+p[1]+" should be a valid identifier.\n")
        print(g+": "+p[1]+" should be a valid identifier.")
        error = error+1
    
    for temp in range(len(p)):
        if("0b" in p[temp] or "0x" in p[temp] or "0o" in p[temp]):
            val = str(eval(p[temp]))
            p.pop(temp)
            p.insert(temp,val)
    
    j=1
    while( j<len(p) ):
    
        if( p[j] == "," ):
            if( not p[j+1].isidentifier() ):
                fp.write(g+": "+p[j+1]+" should be a valid identifier.\n")
                print(g+": "+p[j+1]+" should be a valid identifier.")
                error = error+1
        
        if( p[j].isidentifier() ):
            if( datatype.get(p[j]) == None ):
                datatype[p[j]] = p[0]
            else:
                datatype[p[j]] = p[0]
                if( value.get(p[j]) != None):
                    del value[p[j]]
        
        if( p[j] == "=" ):
            j = eval_expr(j,p,g,datatype,value)
        j=j+1
    
    if(p[-1] != ";"):
        fp.write(g+": Semicolon is missing\n")
        print(g+": Semicolon is missing")
        error = error+1
    #print(value)
    #print(datatype)
        

def check_print(a,index,datatype,value):
    p = a.split()
    g = str(index+1)
    global bracket
    global error
    temp = ""
    
    fo = open("prog.txt","r")
    li = fo.readlines()
    fo.close()
    s = li[index]

    i=0
    while(i<len(s)):
        var = ""
        if( s[i] == "\""):
            i=i+1
            while( s[i] != "\""):
                temp = temp + s[i]
                i=i+1
        
        elif( s[i] == "(" ):
            j= i+1
            while( s[j].isspace() ):
                j= j+1
                continue
            if( s[j] != "\""):
                while( s[j] != "," and s[j] != ")"):
                    if( s[j].isspace() ):
                        j = j+1
                        continue
                    else:
                        var = var + s[j]
                    j=j+1
                    
                if('.' in var):
                    asd = var.index('.')
                    var = var[asd+1:]
                    
                if( datatype.get(var) == None ):
                    fp.write(g+": '"+var+"' undeclared\n")
                    print(g+": '"+var+"' undeclared")
                    error = error+1
                else:
                    if( value.get(var) == None ):
                        fp.write(g+": '"+var+"' uninitialized\n")
                        print(g+": '"+var+"' uninitialized")
                        error = error+1
                    elif( value.get(var) != None ):
                        temp = temp + str(value.get(var))
                i=j
        
        elif( s[i] == "," ):
            i = i+1
            while( s[i] != "," and s[i] != ")"):
                if( s[i].isspace() ):
                    i = i+1
                    continue
                else:
                    var = var + s[i]
                i=i+1
            
            if('.' in var):
                asd = var.index('.')
                var = var[asd+1:]

            if( datatype.get(var) == None ):
                fp.write(g+": '"+var+"' undeclared\n")
                print(g+": '"+var+"' undeclared")
                error = error+1                
            else:
                if( value.get(var) == None ):
                    fp.write(g+": '"+var+"' uninitialized\n")
                    print(g+": '"+var+"' uninitialized")
                    error = error+1
                elif( value.get(var) != None ):
                    temp = temp + str(value.get(var))
        i=i+1
    
    if(p[-1] != ";"):
            fp.write(g+": Semicolon is missing\n")
            print(g+": Semicolon is missing")
            error = error+1    
    
    if(error == 0):
        fp.write(temp)
        print(temp)
        fp.write("\n")
        #print("")
                

def user_input(a,index,datatype,value):
    p = a.split()
    g = str(index+1)
    global bracket
    global error
    temp = ""
    global time_list
    
    fo = open("prog.txt","r")
    li = fo.readlines()
    fo.close()
    s = li[index]
    
    if( datatype.get(p[0]) == None ):
        fp.write(g+": '"+p[0]+"' undeclared\n")
        print(g+": '"+p[0]+"' undeclared")
        error = error+1
        return
    
    start = s.find('"')
    if(start!=-1):
        end = s.find('"',start+1)
        print(s[start+1:end],end='')
    
    if("readFloat" in p):
        try:
            start = time.clock()
            var = float(input())
            time_list.append(time.clock()-start)
        except(ValueError):
            time_list.append(time.clock()-start)
            fp.write(g+": incompatible types while taking input type 'float'\n")
            print(g+": incompatible types while taking input type 'float'")
            error+=1
        else:
            if( datatype.get(p[0])=="int" ):
                value[p[0]] = int(var)                                      
            elif( datatype.get(p[0])=="float" ):
                value[p[0]] = var            
            elif( datatype.get(p[0])=="string" ):
                fp.write(g+": incompatible types when initializing type 'string'\n")
                print(g+": incompatible types when initializing type 'string'")
                error += 1 
        
    elif("readInt" in p):
        start = time.clock()
        var = input()
        time_list.append(time.clock()-start)
        try:
            var = eval(var)
        except(NameError):
            if(len(var) == 1):
                var = ord(var)
                if( datatype.get(p[0])=="int" ):
                    value[p[0]] = var
                elif( datatype.get(p[0])=="float" ):
                    value[p[0]] = float(var)            
                elif( datatype.get(p[0])=="string" ):
                    value[p[0]] = chr(var)
            else:
                fp.write(g+": incompatible types\n")
                print(g+": incompatible types")
                error += 1
        else:
            if( datatype.get(p[0])=="int" ):
                value[p[0]] = int(var)
            elif( datatype.get(p[0])=="float" ):
                value[p[0]] = float(var)            
            elif( datatype.get(p[0])=="string" ):
                value[p[0]] = chr(int(var))            

    elif("readString" in p):
        start = time.clock()
        var = input()
        time_list.append(time.clock()-start)
        if( datatype.get(p[0])=="string" ):
            value[p[0]] = var
        elif( datatype.get(p[0])=="int" ):
            if(len(var) == 1):
                value[p[0]] = ord(var)
            else:
                fp.write(g+": incompatible types 'string' to 'int'\n")
                print(g+": incompatible types 'string' to 'int'")
                error += 1
        elif( datatype.get(p[0])=="float" ):
            fp.write(g+": incompatible types when initializing type 'float'\n")
            print(g+": incompatible types when initializing type 'float'")
            error += 1


if __name__ == '__main__':
    
    start_time = time.clock()
    fo = open("prog.txt","r")
    fp = open("output.txt","w")
    lines = fo.readlines()
    fo.close()
    prnt = lines
    i=0
    bracket=0
    error = 0
    global_value = {}
    global_type = {}
    time_list = []
    
    for q in lines:
        lines[i] = q.strip()
        i=i+1
    
    for q in lines:
        j=0
        r=q
        
        for i in range(1,len(q)):
            if( not q[i].isalnum() and q[i] != " " and q[i] != "_" and (q[i] != "." or q[i-1].isidentifier())):
                
                #if( not q[i-1].isalnum() and q[i-1] != " " ):
                #    continue
                
                if( i == len(q)-1 ):
                    if( q[i-1] == " " ):
                        continue
                    else:
                        r = r[:i+j]+" "+r[i+j:]
                        j=j+1                    
                
                elif( q[i-1] == " " and q[i+1] == " " ):
                    continue
                
                elif( q[i-1] == " " and q[i+1] != " " ):
                    r = r[:i+j+1]+" "+r[i+j+1:]
                    j=j+1
                
                elif( q[i-1] != " " and q[i+1] == " " ):
                    r = r[:i+j]+" "+r[i+j:]
                    j=j+1
                
                else:
                    r = r[:i+j] + " " + r[i+j] + " " + r[i+j+1:]
                    j=j+2
        lines[lines.index(q)] = r
    
    #print(lines)

    pq = 0
    flag = False
    class_flag = False
    for t in lines:
        p = t.split()
        
        if( t == "" ):
            pq=pq+1
            continue
       
        if( flag ):
            if( len(p)>1 and p[-2] == '#' and p[-1] == '>' ):
                flag = False
                pq+=1
            else:       
                pq+=1
            continue
        
        '''if( p[0].isidentifier() and p[1].isidentifier() ):
            if( not isdatatype(p[0]) ):
                fp.write(str(lines.index(t)+1)+": '"+p[0]+"' must be a DATATYPE\n")
                print(str(lines.index(t)+1)+": '"+p[0]+"' must be a DATATYPE")
                error = error+1'''
        
        if( "readInt" in p or "readFloat" in p or "readString" in p ):
            if(class_flag == True):
                user_input(t,pq,class_type,class_value)
            else:
                user_input(t,pq,global_type,global_value)
        
        elif( isdatatype(p[0]) ):
            if(class_flag == True):
                check_declaration(t,pq+1,class_type,class_value)
            else:
                check_declaration(t,pq+1,global_type,global_value)
        
        elif( p[0] == '-' and p[1] == '-' ):
            pq=pq+1
            continue
        
        elif( p[0] == '<' and p[1] == '#' ):
            pq=pq+1
            flag = True
            continue
        
        elif( p[0]=='class' ):
            class_flag = True
            class_name = p[1]
            class_type = {}
            class_value = {}
            
        elif( 'new' in p ):
            class_object = p[0]
        
        elif( p[0] == "{"  and len(p)==1 ):
            bracket = bracket+1
            pq+=1
            continue
        
        elif( p[0] == "}"  and len(p)==1 ):
            bracket = bracket-1
            class_flag = False
            pq+=1
            continue
        
        elif( p[0] == "print" ):
            if(class_flag == True):
                check_print(t,pq,class_type,class_value)
            elif(class_object in p and '.' in p):
                check_print(t,pq,class_type,class_value)
            else:
                check_print(t,pq,global_type,global_value)
        
        else:
            if(class_flag == True):
                check_expr(t,pq+1,class_type,class_value)
            elif( p[1]=='.' ):
                check_expr(t,pq+1,class_type,class_value)
            else:
                check_expr(t,pq+1,global_type,global_value)
        
        pq=pq+1
    
    if(bracket != 0 ):
        fp.write("Unmatched curly brackets found\n")
        error = error +1
    
    fp.write("\n\n")
    fp.write(str(global_type))
    fp.write("\n")
    fp.write(str(global_value))
    end_time = time.clock()
    input_time = sum(time_list)
    run_time = end_time - start_time - input_time
    print("Time in execution is {0:5.3f} mili seconds".format(run_time*1000))
    print("\n\n\n")
    print(global_type)
    print(global_value)
    print(class_type)
    print(class_value)    
    
    fp.close()
