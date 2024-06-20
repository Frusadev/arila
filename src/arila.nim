import strutils
#import strformat

type 
  TokenType = enum 
    PLUS
    MINUS
    DIVIDE
    MULTIPLICATE 
    NUMBER 
    UNKNOWN_TOKEN

type 
  Token = ref object
    value: string
    tokType: TokenType

let 
  operations: seq[TokenType] = @[PLUS, MINUS, DIVIDE, MULTIPLICATE]
  operators: seq[string] = @["+", "-", "/", "*"]
  #digits: seq[char] = @['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']

proc parsingError() =
  raise newException(ValueError, "Error while parsing")

proc matchOperator(input: string) : Token =
  let refinedInput = input.replace(" ", "")
  var 
    outputToken: Token = new(Token)
  outputToken.value = input
  case refinedInput
  of "+":
    outputToken.tokType = PLUS
  of "-":
    outputToken.tokType = MINUS
  of "/":
    outputToken.tokType = DIVIDE
  of "*":
    outputToken.tokType = MULTIPLICATE
  else:
    outputToken.tokTYpe = UNKNOWN_TOKEN
  return outputToken

proc lex(input: string) : seq[Token] =
  let 
    refinedInput: string = input.replace(" ", "")

  var 
    currentTokenWord: string
    rawTokenList: seq[string]
    position: int = 0

  for character in refinedInput:
    inc position
    if $character notin operators:
      currentTokenWord.add(character)
    elif $character in operators and (currentTokenWord.isEmptyOrWhitespace()):
      parsingError()
    elif $character in operators:
      rawTokenList.add(currentTokenWord)
      rawTokenList.add($character)
      currentTokenWord = "" 

  rawTokenList.add(currentTokenWord)
  
  var 
    tokenList: seq[Token]

  for tokenWord in rawTokenList:
    var 
      currentToken: Token = new(Token)

    if not (tokenWord in operators):
      currentToken.value = tokenWord
      currentToken.tokType = NUMBER
      tokenList.add(currentToken)
    else:
      tokenList.add(matchOperator(tokenWord))
  return tokenList

proc calculate(leftNumberToken: Token, operatorToken: Token, rightNumberToken: Token) : float =
  assert operatorToken.tokType in operations
  assert leftNumberToken.tokType == NUMBER
  assert rightNumberToken.tokType == NUMBER

  let 
    left: float = parseFloat(leftNumberToken.value)
    right: float = parseFloat(rightNumberToken.value)

  case operatorToken.tokType 
  of PLUS:
    result = left + right
  of MULTIPLICATE:
    result = left * right
  of MINUS:
    result = left - right
  of DIVIDE:
    result = left / right
  else:
    result = 0

proc calculate(left: float, operatorToken: Token, rightNumberToken: Token) : float =
  assert operatorToken.tokType in operations
  assert rightNumberToken.tokType == NUMBER

  let 
    right: float = parseFloat(rightNumberToken.value)

  case operatorToken.tokType 
  of PLUS:
    result = left + right
  of MULTIPLICATE:
    result = left * right
  of MINUS:
    result = left - right
  of DIVIDE:
    result = left / right
  else:
    result = 0

proc interpret(input: seq[Token]) : float =
  var 
    size: int = len input 

  var
    operator: Token
    position: int = 0 
    previousNumberToken: Token
    previousResult: float
    current: Token
    factorsOutput: seq[Token]
  # For factors : factor (mul | div) factor
  while position <= size - 1:
    current = input[position]
    if current.tokType == NUMBER:
      if position == 0:
        previousNumberToken = current
      else:
        if not (operator.tokType in [DIVIDE, MULTIPLICATE]):
          factorsOutput.add(previousNumberToken)
          factorsOutput.add(operator)
          previousNumberToken = current
        else:
          var 
            tok = new(Token)
            value = $calculate(previousNumberToken, operator, current)
          tok.value = value
          tok.tokType = NUMBER
          previousNumberToken = tok
    if position == input.len - 1:
      if operator.tokType in [MULTIPLICATE, DIVIDE]:
        factorsOutput.add(previousNumberToken)
      else:
        factorsOutput.add(current)
    else: operator = current
    inc position

  position = 0
  size = len factorsOutput

  while position <= size - 1:
    current = factorsOutput[position]
    if current.tokType == NUMBER:
      if position == 0:
        previousNumberToken = current
        previousResult = parseFloat(current.value)
      else:
        previousResult = calculate(previousResult, operator, current)
    else: operator = current
    inc position
  return previousResult

while true:
  write(stdout, "> ")
  let input = readLine(stdin)
  echo interpret(lex(input))
