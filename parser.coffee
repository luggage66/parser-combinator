class Input
	constructor: (@source, @position = 0, @line = 1, @column = 1) ->

	isEqual: (otherInput) ->
		return @source == otherInput.source and @position == otherInput.position

	isAtEnd: () =>
		@position == @source.length

	getCurrent: () =>
		@source.charAt(@position)

	advance: () =>
		if @isAtEnd()
			throw "Already at the end of the atream, can't advnace()"

		newLineNumber = if @getCurrent() == '\n' then @line = 1 else @line
		newColumnNumber = if @getCurrent() == '\n' then 1 else @column + 1

		new Input(@source, @position + 1, newLineNumber, newColumnNumber)

Failure = (remainder, message, expectations) ->
	successful: false
	remainder: remainder
	message: message
	expectations: expectations

Success = (value, remainder) ->
	successful: true
	value: value
	remainder: remainder

DetermineBestError = (firstFailure, secondFailure) ->
	if secondFailure.remainder.position > firstFailure.remainder.position
		return secondFailure

	if secondFailure.remainder.position == firstFailure.remainder.position
		return Failure(
			firstFailure.remainder,
			firstFailure.message,
			firstFailure.expectations.concat(secondFailure.expectations)
		)

	return firstFailure

#takes an undecorated parser (a function) and adds all sorts of goodness to it
Parser = (parser) ->

	parser.TryParse = (inputString) ->
		throw "inputString missing" if not inputString

		parser(new Input(inputString))
	
	parser.Parse = (inputString) ->
		throw "inputString missing" if not inputString

		result = parser.TryParse(inputString)

		return result.value if result.successful

		expectationsMessage =
			if result.expectations
				' expected ' + result.expectations.join(' or ')
			else
				''
		#todo: show recently consumed
		throw "Parsing Failure: #{result.message}; #{expectationsMessage};"

	parser.Or = (second) ->
		throw "no second parser in Or()" if not second

		first = parser #just for clarity

		Parser (i) ->

			firstResult = first(i)

			if not firstResult.successful
				secondResult = second(i)

				if not secondResult.successful
					return DetermineBestError firstResult, secondResult

				return secondResult

			if firstResult.remainder.isEqual(i)
				secondResult = second(i)

				if not secondResult.successful
					return firstResult

				return secondResult

			return firstResult

	parser.Then = (second) ->
		throw "no second parser in Then()" if not second

		first = parser

		Parser (i) ->
			firstResult = first(i)

			if firstResult.successful
				return second(firstResult.value)(firstResult.remainder);

			return firstResult;

	parser.Map = (map) ->
		throw "no map supplied to in Map()" if not map

		parser.Then (x) -> Parse.Return(map(x))

	return parser

Return = (value) ->
	Parser (i) -> Success(value, i)

Parse =
	Char: (predicate, description) ->

		throw "predicate missing" if not predicate
		throw "description missing" if not description

		Parser (i) -> 

			if not i.isAtEnd()
				
				if predicate(i.getCurrent())

					return Success(i.getCurrent(), i.advance())

				return Failure(i, "Unexpected #{i.getCurrent()}", [ description ])

			return Failure(i, "Unexpected end of input reached", [ description ])

	Digit: () ->

		digits = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

		Parse.Char(
			(c) -> digits.indexOf(c) >= 0
			'a digit'
		)

	Return: (value) ->
		Parser (i) ->
			Success(value, i)

# END PARSER LIBRARY, BEGIN TEST

# define the grammar, a digit followed by a semicolor

semicolon = Parse.Char(
	(x) -> x == ';'
	'a semicolon'
)

singleDigit = Parse.Digit().Map((d) -> 'Number: ' + d)

grammar = singleDigit.Then (x) ->
	semicolon

#should succeed
console.log '-------------Test 1, should succeed ------------------'
console.log grammar.Parse("3;")

#should fail
console.log '-------------Test 2, should fail ------------------'
console.log grammar.Parse("x")

#console.log Parse.Parse(grammar, stringToParse)
