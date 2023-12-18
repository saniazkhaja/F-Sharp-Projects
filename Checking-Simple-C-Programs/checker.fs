//
// Analyzer for simple C programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid simple C program.
//
// Modified by:
//   << Sania Khaja >>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module checker =
  //
  // NOTE: all functions in the module must be indented.
  //
  //
  // matchToken
  //
  let private matchToken expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

    
  // calls recursion on tokens from stmts until no more statements found
  // this is for different statements such as empty, int, cin, cout, =, if
  // <stmt>
  let rec private stmt tokens symbolTable = 
    let next_token = List.head tokens  // token we starting with
    let tail_token = List.head (List.tail tokens)  // token after the one we are on

    // parse empty semicolon
    if next_token = ";" then
      empty tokens symbolTable
      
    // parse declaration
    else if next_token = "int" then
      vardecl tokens symbolTable

    // parse declaration
    else if next_token = "real" then        
      vardecl tokens symbolTable
     
    // parse input
    else if next_token = "cin" then        
      input tokens symbolTable
      
    // parse output
    else if next_token = "cout" then        
      output tokens symbolTable

    // parse assignment
    else if tail_token = "=" then
      assignment tokens symbolTable
     
    // if statement
    else if next_token = "if" then
      ifstmt tokens symbolTable

    // invalid order
    else if tail_token = "if" then
      if next_token.Contains "identifier" then
        let T2 = List.tail tokens
        let T3 = matchToken "=" T2
        stmt T2 symbolTable
      else
        let T2 = List.tail tokens
        stmt T2 symbolTable
    else if next_token.Contains "identifier" then
      if tail_token.Contains "identifier" then
        let T2 = List.tail tokens
        let T3 = matchToken "=" T2  // seeing equal is in right place
        stmt T2 symbolTable
      else
        (tokens, symbolTable)
    // no change needed. Usually means that end of function body reached
    else
      (tokens, symbolTable)

  // in the case for empty so ;
  // for <empty>
  and private empty tokens symbolTable =
    let tok = matchToken ";" tokens
    stmt tok symbolTable
    
  // for var declaration
  // checking if there is a valid var decl statement by checking int then some name then ;
  // does this through parsing
  // calls stmt tok to go through rest of statemnet in function body
  // <vardecl>
  and private vardecl tokens symbolTable = 
    // let T2 = matchToken "int" tokens  // checking match 
    // let currTok = List.head T2
    // let T3 = List.tail T2
    // let tok = matchToken ";" T3
    let currTok = List.head tokens  // checking match 
    if currTok = "int"  then
       let T2 = matchToken "int" tokens  // checking match 
       //let identifier = (List.head T2).Split ":"
       let parts = (List.head T2).Split ":"
       let identifier = parts.[1].Trim()
       let newSymbolTable = (identifier, "int")::symbolTable
       let T3 = List.tail T2
       let tok = matchToken ";" T3
       stmt tok symbolTable
    else if currTok = "real" then
      let T2 = matchToken "real" tokens  // checking match 
      let parts = (List.head T2).Split ":"
      let identifier = parts.[1].Trim()
      let newSymbolTable = (identifier, "real")::symbolTable
      let T3 = List.tail T2
      if List.head T3 = "unknown:." then
        let T4 = List.tail T3
        let T5 = List.tail T4
        let tok = matchToken ";" T5
        stmt tok symbolTable
      else 
        let tok = matchToken ";" T3
        stmt tok symbolTable
    else
      let T2 = matchToken "int or real" tokens  // checking match 
      let T3 = List.tail T2
      let tok = matchToken ";" T3
      stmt tok symbolTable
    
  // for inputs
  // checking for valid input by checking cin then >> then making sure it is being stores in an identifier
  // does this through parsing
  // calls stmt tok to go through rest of statemnet in function body
  // <input>
  and private input tokens symbolTable = 
    let T2 = matchToken "cin" tokens   
    let T3 = matchToken ">>" T2
    let currTok = List.head T3
    // checking if storing input into identifier
    if currTok.Contains "identifier" then
      let parts = currTok.Split ":"
      let identifier = parts.[1].Trim()
      if (not (List.contains (identifier, "int") symbolTable) && not(List.contains (identifier, "real") symbolTable)) then
        failwith ("variable '" + identifier + "' undefined")
      let T4 = List.tail T3
      let tok = matchToken ";" T4
      stmt tok symbolTable
    else  // not storing into identifier which is an issue and error
      let T4 = matchToken "identifier" T3
      let tok = matchToken ";" T4
      stmt tok symbolTable

  // for outputs
  // checking for valid input by checking cout then << then calling another function to check valid output
  // does this through parsing
  // <output>
  and private output tokens symbolTable =
    let T2 = matchToken "cout" tokens  // matching check
    let T3 = matchToken "<<" T2
    let currTok = List.head T3
    // checking if valid output
    outputValue currTok T3 symbolTable

  // for valid output value
  // checking for valid input by calling another function to see if there is a valid expression value and then parses for ;
  // does this through parsing
  // calls stmt tok to go through rest of statemnet in function body
  // <output-value>
  and private outputValue currTok T3 symbolTable =
    let valid = exprValue currTok
    if currTok.Contains "identifier" then
      let parts = currTok.Split ":"
      let identifier = parts.[1].Trim()
      if (not (List.contains (identifier, "int") symbolTable) && not(List.contains (identifier, "real") symbolTable)) then
        failwith ("variable '" + identifier + "' undefined")
    // valid ouput
    if valid = true then
      let T4 = List.tail T3
      if List.head T4 = "unknown:." then
        let T5 = List.tail T4
        let T6 = List.tail T5
        let tok = matchToken ";" T6
        stmt tok symbolTable
      else 
        let tok = matchToken ";" T4
        stmt tok symbolTable
    else  // not valid ouput so show error through matching
      let T4 = matchToken "identifier or literal" T3
      let tok = matchToken ";" T4
      stmt tok symbolTable

  // for valid expression values
  // returns true or false depending on if expression value is valid or not
  // <expr-value>
  and private exprValue currTok =
    if currTok.Contains "identifier" || currTok.Contains "int_literal" || currTok.Contains "real_literal" || currTok.Contains "str_literal" || currTok.Contains "true" || currTok.Contains "false" || currTok.Contains "endl" then
      true
    else
      false
      
  // for assigments
  // checking for valid assigment checking if before is an identifier then = and then calls function to see if there is a valid expression after =
  // does this through parsing
  // calls stmt tok to go through rest of statemnet in function body if there was no identifier
  // <assignment>
  and private assignment tokens symbolTable =
      let next_token = List.head tokens  // token we starting with
      // checking if starts with identifier
      if next_token.Contains "identifier" then
        let parts = next_token.Split ":"
        let identifier = parts.[1].Trim()
        if (not (List.contains (identifier, "int") symbolTable) && not(List.contains (identifier, "real") symbolTable)) then
          failwith ("variable '" + identifier + "' undefined")
        let T2 = List.tail tokens
        let T3 = matchToken "=" T2  // seeing equal is in right place
        let endStatement = ";"  // what the statement should end with to be valid
        expr T3 endStatement symbolTable identifier
      else  // no identfier so error shown through matchToken
        let T2 = matchToken "identifier" tokens
        let tok = matchToken ";" T2
        stmt tok symbolTable


  // for expressions
  // checking for valid expressions which is either just a expression value, or expression value then expression operation then expression value
  // making sure it ends with the valid end statement, so either ) or ;
  // calling seperate functions to check for valid expression values and valid operation
  // does this through parsing
  // calls stmt tok to go through rest of statemnet in function body
  // <expr>
  and private expr T3 endStatement symbolTable identifier =
    let currTok = List.head T3  // after equal sign value
    let afterOp = currTok
    let currTailHd = List.head (List.tail T3)  // used to know if there is any expression operations
    let T4 = List.tail T3
    let T5 = List.tail T4
    let afterOp1 = List.head T5
    let operator = currTailHd
    if currTok.Contains "identifier" then
      let parts = currTok.Split ":"
      let identifier = parts.[1].Trim()
      if (not (List.contains (identifier, "int") symbolTable) && not(List.contains (identifier, "real") symbolTable)) then
        failwith ("variable '" + identifier + "' undefined")
    // checking for valid expression
    let validExp = exprValue currTok
    if (validExp = true) then
      let validSign1 = exprOp currTailHd
      // seeing if there is more of an expression or not
      if endStatement = ";" && validSign1 = false then  // so dealing with assignments
        if (List.contains (identifier, "int") symbolTable) then
          if afterOp.Contains "identifier" then
            let partsN = afterOp.Split ":"
            let identifierN = partsN.[1].Trim()
            if (not(afterOp.Contains "int_literal") && not(List.contains(identifierN, "int") symbolTable)) then
              let id_type = "int"
              if afterOp.Contains "identifier" then 
                let expr_type = "real"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'") 
              if afterOp.Contains "real_literal" then 
                let expr_type = "real"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'") 
              if afterOp.Contains "str_literal" then 
                let expr_type = "str"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'") 
              if afterOp.Contains "false" || afterOp.Contains "true" then 
                let expr_type = "bool"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'")
          else 
            let identifierN = "NA"
            if (not(afterOp.Contains "int_literal") && not(List.contains(identifierN, "int") symbolTable)) then
              let id_type = "int"
              if afterOp.Contains "identifier" then 
                let expr_type = "real"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'") 
              if afterOp.Contains "real_literal" then 
                let expr_type = "real"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'") 
              if afterOp.Contains "str_literal" then 
                let expr_type = "str"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'") 
              if afterOp.Contains "false" || afterOp.Contains "true" then 
                let expr_type = "bool"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'")

        if (List.contains (identifier, "real") symbolTable) then
          if afterOp.Contains "identifier" then
            let partsN = afterOp.Split ":"
            let identifierN = partsN.[1].Trim()
            if (not(afterOp.Contains "real_literal") && not(List.contains(identifierN, "real") symbolTable) && not(afterOp.Contains "int_literal") && not(List.contains(identifierN, "int") symbolTable)) then
              let id_type = "real" 
              if afterOp.Contains "str_literal" then 
                let expr_type = "str"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'") 
              if afterOp.Contains "false" || afterOp.Contains "true" then 
                let expr_type = "bool"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'")
          else 
            let identifierN = "NA"
            if (not(afterOp.Contains "real_literal") && not(List.contains(identifierN, "real") symbolTable) && not(afterOp.Contains "int_literal") && not(List.contains(identifierN, "int") symbolTable)) then
              let id_type = "real" 
              if afterOp.Contains "str_literal" then 
                let expr_type = "str"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'") 
              if afterOp.Contains "false" || afterOp.Contains "true" then 
                let expr_type = "bool"
                failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'")
        
        
      
      // old code
      if currTailHd = endStatement then
        if (identifier = "NA") then  
          if afterOp.Contains "identifier" then
            let partsN = afterOp.Split ":"
            let identifierN = partsN.[1].Trim()
            if List.contains(identifierN, "int") symbolTable then
              let expr_type = "int"
              failwith ("if condition must be 'bool', but found '" + expr_type + "'")
            if List.contains(identifierN, "real") symbolTable then
              let expr_type = "real"
              failwith ("if condition must be 'bool', but found '" + expr_type + "'")
          if afterOp.Contains "int_literal" then 
            let expr_type = "int"
            failwith ("if condition must be 'bool', but found '" + expr_type + "'")  
          if afterOp.Contains "real_literal" then 
            let expr_type = "real"
            failwith ("if condition must be 'bool', but found '" + expr_type + "'") 
          if afterOp.Contains "str_literal" then 
            let expr_type = "str"
            failwith ("if condition must be 'bool', but found '" + expr_type + "'")
        let T4 = List.tail T3
        if List.head T4 = "unknown:." then
          let T5 = List.tail T4
          let T6 = List.tail T5
          let tok = matchToken endStatement T6
          stmt tok symbolTable
        else 
          let tok = matchToken endStatement T4
          stmt tok symbolTable
      else  // there is more to evaluate due to a sign or another value existing
        // checking for valid sign in expression
        let validSign = exprOp currTailHd
        if validSign = true then
          let T4 = List.tail T3
          let T5 = List.tail T4
          let currTokNew = List.head T5
          
          if currTokNew.Contains "identifier" && currTailHd = "==" then
            let partsN = currTokNew.Split ":"
            let identifierN = partsN.[1].Trim()
            if List.contains (identifierN, "real") symbolTable then
              printfn "warning: comparing real numbers with == may never be true"
          if (currTokNew.Contains "real_literal" && currTailHd = "==") then
            printfn "warning: comparing real numbers with == may never be true"
            
          if (currTailHd = "+" || currTailHd = "-" || currTailHd = "*" || currTailHd = "/" || currTailHd = "^") then  // arithmeic operations
            if (not(currTok.Contains "identifier") && not(currTok.Contains "int_literal") && not(currTok.Contains "real_literal")) then  // first operand is not valid at all
              failwith ("operator " + currTailHd + " must involve 'int' or 'real'")
            else if (not(currTokNew.Contains "identifier") && not(currTokNew.Contains "int_literal") && not(currTokNew.Contains "real_literal")) then  // second operand not valid at all
              failwith ("operator " + currTailHd + " must involve 'int' or 'real'")  
            // if statement        
            if (identifier = "NA") then  
              if afterOp.Contains "identifier" then
                let partsN = afterOp.Split ":"
                let identifierN = partsN.[1].Trim()
                if List.contains(identifierN, "int") symbolTable then
                  let expr_type = "int"
                  failwith ("if condition must be 'bool', but found '" + expr_type + "'")
                if List.contains(identifierN, "real") symbolTable then
                  let expr_type = "real"
                  failwith ("if condition must be 'bool', but found '" + expr_type + "'")
              if afterOp.Contains "int_literal" then 
                let expr_type = "int"
                failwith ("if condition must be 'bool', but found '" + expr_type + "'")  
              if afterOp.Contains "real_literal" then 
                let expr_type = "real"
                failwith ("if condition must be 'bool', but found '" + expr_type + "'") 
              if afterOp.Contains "str_literal" then 
                let expr_type = "str"
                failwith ("if condition must be 'bool', but found '" + expr_type + "'")
          else if (currTailHd = "<" || currTailHd = "<=" || currTailHd = ">" || currTailHd = ">=" || currTailHd = "==" || currTailHd = "!=") then
            
              
            if currTok.Contains "identifier" then
              let parts = currTok.Split ":"
              let identifier = parts.[1].Trim()
              if afterOp1.Contains "identifier" then
                let partsN = afterOp1.Split ":"
                let identifierN = partsN.[1].Trim()
                if (List.contains(identifier, "int") symbolTable && not(List.contains(identifierN, "int") symbolTable)) then
                  let left_type = "int"
                  if List.contains(identifierN, "real") symbolTable then
                    let right_type = "real"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  // if afterOp.Contains "int_literal" then 
                  //   let right_type = "int"
                  //   failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")  
                  if afterOp1.Contains "real_literal"  && currTailHd <> "==" then 
                    let right_type = "real"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if afterOp1.Contains "str_literal" then 
                    let right_type = "str"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if afterOp1.Contains "false" || afterOp1.Contains "true" then 
                    let right_type = "bool"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                if (List.contains(identifier, "real") symbolTable && not(List.contains(identifierN, "real") symbolTable)) then
                  let left_type = "real"
                  if List.contains(identifierN, "int") symbolTable then
                    let right_type = "int"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if afterOp1.Contains "int_literal" then 
                    let right_type = "int"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                // if afterOp.Contains "real_literal" then 
                //   let right_type = "real"
                //   failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if afterOp1.Contains "str_literal" then 
                    let right_type = "str"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if afterOp1.Contains "false" || afterOp1.Contains "true" then 
                    let right_type = "bool"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
              else  // second operand is not a variable
                if (List.contains(identifier, "int") symbolTable && not(afterOp1.Contains "int_literal")) then
                  let left_type = "int"
                  if afterOp1.Contains "real_literal" then 
                    let right_type = "real"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if afterOp1.Contains "str_literal" then 
                    let right_type = "str"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if afterOp1.Contains "false" || afterOp1.Contains "true" then 
                    let right_type = "bool"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                if (List.contains(identifier, "real") symbolTable && not(afterOp1.Contains "real_literal")) then
                  let left_type = "real"
                  
                  if afterOp1.Contains "int_literal" then 
                    let right_type = "int"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if afterOp1.Contains "str_literal" then 
                    let right_type = "str"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if afterOp1.Contains "false" || afterOp1.Contains "true" then 
                    let right_type = "bool"
                    failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                    
            else if afterOp1.Contains "identifier" then
              let parts = afterOp1.Split ":"
              let identifier = parts.[1].Trim()
              if currTok.Contains "identifier" then
                let partsN = currTok.Split ":"
                let identifierN = partsN.[1].Trim()
                if (List.contains(identifier, "int") symbolTable && not(List.contains(identifierN, "int") symbolTable)) = false then
                  let left_type = "int"
                  if List.contains(identifierN, "real") symbolTable then
                    let right_type = "real"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                  // if afterOp.Contains "int_literal" then 
                  //   let right_type = "int"
                  //   failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")  
                  if currTok.Contains "real_literal"  && currTailHd <> "==" then 
                    let right_type = "real"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                  if currTok.Contains "str_literal" then 
                    let right_type = "str"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                  if currTok.Contains "false" || currTok.Contains "true" then 
                    let right_type = "bool"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                if (List.contains(identifier, "real") symbolTable && not(List.contains(identifierN, "real") symbolTable)) then
                  let left_type = "real"
                  if List.contains(identifierN, "int") symbolTable then
                    let right_type = "int"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                  if currTok.Contains "int_literal" then 
                    let right_type = "int"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                // if afterOp.Contains "real_literal" then 
                //   let right_type = "real"
                //   failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
                  if currTok.Contains "str_literal" then 
                    let right_type = "str"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                  if currTok.Contains "false" || currTok.Contains "true" then 
                    let right_type = "bool"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
              else  // second operand is not a variable
                if (List.contains(identifier, "int") symbolTable && not(currTok.Contains "int_literal")) then
                  let left_type = "int"
                  
                  if currTok.Contains "real_literal" then 
                    let right_type = "real"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                  if currTok.Contains "str_literal" then 
                    let right_type = "str"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                  if currTok.Contains "false" || currTok.Contains "true" then 
                    let right_type = "bool"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                if (List.contains(identifier, "real") symbolTable && not(currTok.Contains "real_literal")) then
                  let left_type = "real"
                  
                  if currTok.Contains "int_literal" then 
                    let right_type = "int"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                  if currTok.Contains "str_literal" then 
                    let right_type = "str"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
                  if currTok.Contains "false" || currTok.Contains "true" then 
                    let right_type = "bool"
                    failwith ("type mismatch '" + right_type + "' " + operator + " '" + left_type + "'")
            else if currTok.Contains "int_literal" then
              let left_type = "int"
              if afterOp1.Contains "real_literal" then 
                let right_type = "real"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
              if afterOp1.Contains "str_literal" then 
                let right_type = "str"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
              if afterOp1.Contains "false" || afterOp1.Contains "true" then 
                let right_type = "bool"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
            else if currTok.Contains "real_literal" then
              let left_type = "real"
              if afterOp1.Contains "int_literal" then 
                let right_type = "int"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
              if afterOp1.Contains "str_literal" then 
                let right_type = "str"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
              if afterOp1.Contains "false" || afterOp1.Contains "true" then 
                let right_type = "bool"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
            else if currTok.Contains "str_literal" then
              let left_type = "str"
              if afterOp1.Contains "real_literal" then 
                let right_type = "real"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
              if afterOp1.Contains "int_literal" then 
                let right_type = "int"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
              if afterOp1.Contains "false" || afterOp1.Contains "true" then 
                let right_type = "bool"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
            else if currTok.Contains "false" || currTok.Contains "true" then
              let left_type = "bool"
              if afterOp1.Contains "real_literal" then 
                let right_type = "real"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
              if afterOp1.Contains "str_literal" then 
                let right_type = "str"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")
              if afterOp1.Contains "int_literal" then 
                let right_type = "int"
                failwith ("type mismatch '" + left_type + "' " + operator + " '" + right_type + "'")

            if (List.contains (identifier, "int") symbolTable) then
              let id_type = "int" 
              let expr_type = "bool"
              failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'")
            if (List.contains (identifier, "real") symbolTable) then
              let id_type = "real" 
              let expr_type = "bool"
              failwith ("cannot assign '" + expr_type + "' to variable of type '" + id_type + "'")
          else 
            // if statement        
            if (identifier = "NA") then  
              if afterOp.Contains "identifier" then
                let partsN = afterOp.Split ":"
                let identifierN = partsN.[1].Trim()
                if List.contains(identifierN, "int") symbolTable then
                  let expr_type = "int"
                  failwith ("if condition must be 'bool', but found '" + expr_type + "'")
                if List.contains(identifierN, "real") symbolTable then
                  let expr_type = "real"
                  failwith ("if condition must be 'bool', but found '" + expr_type + "'")
              if afterOp.Contains "int_literal" then 
                let expr_type = "int"
                failwith ("if condition must be 'bool', but found '" + expr_type + "'")  
              if afterOp.Contains "real_literal" then 
                let expr_type = "real"
                failwith ("if condition must be 'bool', but found '" + expr_type + "'") 
              if afterOp.Contains "str_literal" then 
                let expr_type = "str"
                failwith ("if condition must be 'bool', but found '" + expr_type + "'")
            
          
          if currTokNew.Contains "identifier" then
            let parts = currTokNew.Split ":"
            let identifier = parts.[1].Trim()
            if (not (List.contains (identifier, "int") symbolTable) && not(List.contains (identifier, "real") symbolTable)) then
              failwith ("variable '" + identifier + "' undefined")
          // checking for valid expression after sign
          let validAfterExp = exprValue currTokNew
          if validAfterExp = true then
            let T6 = List.tail T5
            if List.head T6 = "unknown:." then
              let T7 = List.tail T6
              let T8 = List.tail T7
              let tok = matchToken endStatement T8
              stmt tok symbolTable
            else 
              let tok = matchToken endStatement T6
              stmt tok symbolTable
          else  // invalid expression after sign
            let T6 = matchToken "identifier or literal" T5
            let tok = matchToken endStatement T6
            stmt tok symbolTable
        else  // not a valid sign
          let T4 = List.tail T3
          let tok = matchToken endStatement T4
          stmt tok symbolTable
    else  // not valid expression
      let T4 = matchToken "identifier or literal" T3
      let tok = matchToken endStatement T4
      stmt tok symbolTable


  // for expression operations
  // checking for valid expression operations and then returns true if valid and false if not valid
  // <expr-op>
  and private exprOp currSign =
    if currSign = "+" || currSign = "-" || currSign = "*" || currSign = "/" || currSign = "^" || currSign = "<" || currSign = "<=" || currSign = ">" || currSign = ">=" || currSign = "==" || currSign = "!=" then
      true
    else
      false


  // for if statements
  // checking for valid if statement by seeing if then ( and then checking if what is after ( is a valid condition
  // calls thenPart function to check if everything in if statement body is valid
  // calls else after in case it exists
  // <ifstmt>
  and private ifstmt tokens symbolTable =
    let T2 = matchToken "if" tokens  // matching check
    let T3 = matchToken "(" T2  // matching check 
    let endStatement = ")"
    let (tok, symbolTable1) = condition T3 endStatement symbolTable
    let (thenTok, symbols) = thenPart tok symbolTable
    let (elseTok, symbolTable2) = elsePart thenTok symbols
    (elseTok, symbolTable)


  // for if conditions
  // calls expr with correct parameters to see if the if statement has valid condition in it's ()
  // <condition>
  and private condition tokens endStatement symbolTable =
    let (tok, symbolTable1) = expr tokens endStatement symbolTable "NA"
    (tok, symbolTable)

  
  // for if and after then statements
  // checking the if statement body to make sure it valid
  // does this through calling stmt to check statements in the if statement body
  // <then-part>
  and private thenPart tokens symbolTable =
    stmt tokens symbolTable
    


  // for else part of if statements
  // if else exists (since else is optional) then makes sure else is not empty. If it is empty then there is an error. It calls the rest stmt to look through any statements in else
  // Otherwise, we just return tokens since there is no else to deal with and no change needs to be made
  // <else-part>
  and private elsePart tokens symbolTable =
    if List.head tokens = "else" then
      let T2 = List.tail tokens
      // empty else
      if List.head T2 = "}" then
        failwith("expecting statement, but found }")
      stmt T2 symbolTable
    else
      (tokens, symbolTable)
    
    
  // checking for empty main body and if not empty then parsing body
  // calls stmt to parse body of function
  // if body has been parsed then returns tokens so that simpleC can take over
  // this is for more statements
  // <morestmts>
  let rec private morestmts tokens originalToken symbolTable = 
    // main body empty
    if List.head tokens = List.head originalToken then
      matchToken "statement" tokens
    else if List.head tokens = "}" then  // reached end of body
      tokens
    else  // main body not empty
      let (updatedTok, symbolTable1) = stmt tokens symbolTable
      morestmts updatedTok originalToken symbolTable

  // calling stmt and then passes what is returned into morestmts
  // <stmts>
  let private stmts tokens symbolTable =  
    let (updatedTok, symbolTable1)  = stmt tokens symbolTable
    let tok = morestmts updatedTok tokens symbolTable
    (tok, symbolTable)


  let private simpleC tokens symboltable = 
    let T2 = matchToken "void" tokens 
    let T3 = matchToken "main" T2 
    let T4 = matchToken "(" T3 
    let T5 = matchToken ")" T4 
    let T6 = matchToken "{" T5 
    let (T7, symbolTable) = stmts T6 symboltable
    let T8 = matchToken "}" T7 
    let T9 = matchToken "$" T8   // $ => EOF, there should be no more tokens T9 
    T9


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message
