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

Char = (predicate, description) ->

	throw "predicate missing" if not predicate
	throw "description missing" if not description

	return (i) -> 

		if not input.isAtEnd()
			
			if predicate(i.getCurrent())

				return Success(i.getCurrent(), i.advance())

			return Failure(i, "Unexpected #{i.getCurrent()}", [ description ])

		return Failure(i, "Unexpected end of input reached", [ description ])

Or = (first, second) ->
	return (i) ->
		firstResult = first(i)

		return 

TryParse = (grammar, inputString) ->
	throw "grammar missing" if not grammar
	throw "inputString missing" if not inputString

	grammar(new Input(inputString))

Parse = (grammar, inputString) ->
	throw "grammar missing" if not grammar
	throw "inputString missing" if not inputString

	result = TryParse(grammar, inputString)

	return result.value if result.successful

	expectationsMessage =
		if result.expectations
			' expected ' + result.expectations.join(' or ')
		else
			''
	#todo: show recently consumed
	throw "Parsing Failure: #{result.message}; #{expectationsMessage};"


stringToParse =
	"""
	5
	"""

input = new Input(stringToParse)

grammar = Char(((x) -> x == '5'), 'a character')

console.log Parse(grammar, stringToParse)

