//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// <<Sania Khaja>>
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.

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
  let rec private stmt tokens = 
    let next_token = List.head tokens  // token we starting with
    let tail_token = List.head (List.tail tokens)  // token after the one we are on

    // parse empty semicolon
    if next_token = ";" then
      empty tokens
      
    // parse declaration
    else if next_token = "int" then        
      vardecl tokens

    // parse declaration
    else if next_token = "real" then        
      vardecl tokens
     
    // parse input
    else if next_token = "cin" then        
      input tokens
      
    // parse output
    else if next_token = "cout" then        
      output tokens  

    // parse assignment
    else if tail_token = "=" then
      assignment tokens
     
    // if statement
    else if next_token = "if" then
      ifstmt tokens

    // invalid order
    else if tail_token = "if" then
      if next_token.Contains "identifier" then
        let T2 = List.tail tokens
        let T3 = matchToken "=" T2
        stmt T2
      else
        let T2 = List.tail tokens
        stmt T2
    else if next_token.Contains "identifier" then
      if tail_token.Contains "identifier" then
        let T2 = List.tail tokens
        let T3 = matchToken "=" T2  // seeing equal is in right place
        stmt T2
      else
        tokens
    // no change needed. Usually means that end of function body reached
    else
      tokens

  // in the case for empty so ;
  // for <empty>
  and private empty tokens =
    let tok = matchToken ";" tokens
    stmt tok
    
  // for var declaration
  // checking if there is a valid var decl statement by checking int then some name then ;
  // does this through parsing
  // calls stmt tok to go through rest of statemnet in function body
  // <vardecl>
  and private vardecl tokens = 
    // let T2 = matchToken "int" tokens  // checking match 
    // let currTok = List.head T2
    // let T3 = List.tail T2
    // let tok = matchToken ";" T3
    let currTok = List.head tokens  // checking match 
    if currTok = "int"  then
       let T2 = matchToken "int" tokens  // checking match 
       let T3 = List.tail T2
       let tok = matchToken ";" T3
       stmt tok
    else if currTok = "real" then
      let T2 = matchToken "real" tokens  // checking match 
      let T3 = List.tail T2
      if List.head T3 = "unknown:." then
        let T4 = List.tail T3
        let T5 = List.tail T4
        let tok = matchToken ";" T5
        stmt tok
      else 
        let tok = matchToken ";" T3
        stmt tok
    else
      let T2 = matchToken "int or real" tokens  // checking match 
      let T3 = List.tail T2
      let tok = matchToken ";" T3
      stmt tok
    
  // for inputs
  // checking for valid input by checking cin then >> then making sure it is being stores in an identifier
  // does this through parsing
  // calls stmt tok to go through rest of statemnet in function body
  // <input>
  and private input tokens = 
    let T2 = matchToken "cin" tokens   
    let T3 = matchToken ">>" T2
    let currTok = List.head T3
    // checking if storing input into identifier
    if currTok.Contains "identifier" then
      let T4 = List.tail T3
      let tok = matchToken ";" T4
      stmt tok
    else  // not storing into identifier which is an issue and error
      let T4 = matchToken "identifier" T3
      let tok = matchToken ";" T4
      stmt tok

  // for outputs
  // checking for valid input by checking cout then << then calling another function to check valid output
  // does this through parsing
  // <output>
  and private output tokens =
    let T2 = matchToken "cout" tokens  // matching check
    let T3 = matchToken "<<" T2
    let currTok = List.head T3
    // checking if valid output
    outputValue currTok T3

  // for valid output value
  // checking for valid input by calling another function to see if there is a valid expression value and then parses for ;
  // does this through parsing
  // calls stmt tok to go through rest of statemnet in function body
  // <output-value>
  and private outputValue currTok T3 =
    let valid = exprValue currTok
    // valid ouput
    if valid = true then
      let T4 = List.tail T3
      if List.head T4 = "unknown:." then
        let T5 = List.tail T4
        let T6 = List.tail T5
        let tok = matchToken ";" T6
        stmt tok
      else 
        let tok = matchToken ";" T4
        stmt tok
    else  // not valid ouput so show error through matching
      let T4 = matchToken "identifier or literal" T3
      let tok = matchToken ";" T4
      stmt tok

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
  and private assignment tokens =
      let next_token = List.head tokens  // token we starting with
      // checking if starts with identifier
      if next_token.Contains "identifier" then
        let T2 = List.tail tokens
        let T3 = matchToken "=" T2  // seeing equal is in right place
        let endStatement = ";"  // what the statement should end with to be valid
        expr T3 endStatement
      else  // no identfier so error shown through matchToken
        let T2 = matchToken "identifier" tokens
        let tok = matchToken ";" T2
        stmt tok


  // for expressions
  // checking for valid expressions which is either just a expression value, or expression value then expression operation then expression value
  // making sure it ends with the valid end statement, so either ) or ;
  // calling seperate functions to check for valid expression values and valid operation
  // does this through parsing
  // calls stmt tok to go through rest of statemnet in function body
  // <expr>
  and private expr T3 endStatement =
    let currTok = List.head T3  // after equal sign value
    let currTailHd = List.head (List.tail T3)  // used to know if there is any expression operations
    // checking for valid expression
    let validExp = exprValue currTok
    if (validExp = true) then
      // seeing if there is more of an expression or not
      if currTailHd = endStatement then
        let T4 = List.tail T3
        if List.head T4 = "unknown:." then
          let T5 = List.tail T4
          let T6 = List.tail T5
          let tok = matchToken endStatement T6
          stmt tok
        else 
          let tok = matchToken endStatement T4
          stmt tok
      else  // there is more to evaluate due to a sign or another value existing
        // checking for valid sign in expression
        let validSign = exprOp currTailHd
        if validSign = true then
          let T4 = List.tail T3
          let T5 = List.tail T4
          let currTokNew = List.head T5
          // checking for valid expression after sign
          let validAfterExp = exprValue currTokNew
          if validAfterExp = true then
            let T6 = List.tail T5
            if List.head T6 = "unknown:." then
              let T7 = List.tail T6
              let T8 = List.tail T7
              let tok = matchToken endStatement T8
              stmt tok
            else 
              let tok = matchToken endStatement T6
              stmt tok
          else  // invalid expression after sign
            let T6 = matchToken "identifier or literal" T5
            let tok = matchToken endStatement T6
            stmt tok  
        else  // not a valid sign
          let T4 = List.tail T3
          let tok = matchToken endStatement T4
          stmt tok  
    else  // not valid expression
      let T4 = matchToken "identifier or literal" T3
      let tok = matchToken endStatement T4
      stmt tok  


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
  and private ifstmt tokens =
    let T2 = matchToken "if" tokens  // matching check
    let T3 = matchToken "(" T2  // matching check 
    let endStatement = ")"
    let tok = condition T3 endStatement
    let thenTok = thenPart tok
    elsePart thenTok


  // for if conditions
  // calls expr with correct parameters to see if the if statement has valid condition in it's ()
  // <condition>
  and private condition tokens endStatement =
    expr tokens endStatement

  
  // for if and after then statements
  // checking the if statement body to make sure it valid
  // does this through calling stmt to check statements in the if statement body
  // <then-part>
  and private thenPart tokens =
    stmt tokens


  // for else part of if statements
  // if else exists (since else is optional) then makes sure else is not empty. If it is empty then there is an error. It calls the rest stmt to look through any statements in else
  // Otherwise, we just return tokens since there is no else to deal with and no change needs to be made
  // <else-part>
  and private elsePart tokens =
    if List.head tokens = "else" then
      let T2 = List.tail tokens
      // empty else
      if List.head T2 = "}" then
        failwith("expecting statement, but found }")
      stmt T2
    else
      tokens
    
    
  // checking for empty main body and if not empty then parsing body
  // calls stmt to parse body of function
  // if body has been parsed then returns tokens so that simpleC can take over
  // this is for more statements
  // <morestmts>
  let rec private morestmts tokens originalToken = 
    // main body empty
    if List.head tokens = List.head originalToken then
      matchToken "statement" tokens
    else if List.head tokens = "}" then  // reached end of body
      tokens
    else  // main body not empty
      let updatedTok = stmt tokens
      morestmts updatedTok originalToken

  // calling stmt and then passes what is returned into morestmts
  // <stmts>
  let private stmts tokens =  
    let updatedTok = stmt tokens
    morestmts updatedTok tokens
  
  //
  // simpleC
  // checking everything of the main function is in correct format
  // calls another function stmts to check the body of main for validity
  // parses and checks void then main then ( then ) then { then the stmts for the body then } then making sure the end has been reached with $ since no value can be after }. T9 is the end
  // <simpleC>
  let private simpleC tokens = 
    let T2 = matchToken "void" tokens 
    let T3 = matchToken "main" T2 
    let T4 = matchToken "(" T3 
    let T5 = matchToken ")" T4 
    let T6 = matchToken "{" T5 
    let T7 = stmts T6
    let T8 = matchToken "}" T7 
    let T9 = matchToken "$" T8   // $ => EOF, there should be no more tokens T9 
    T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message


