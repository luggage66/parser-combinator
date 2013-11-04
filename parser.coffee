class Input
	constructor: (@source, @position = 0, @line = 1, @column = 1) ->

	isEqual: (otherInput) ->
		return @source == otherInput.source and @position == otherInput.position

	isAtEnd: () =>
		@position == @source.length

	getCurrent: () =>
		@source.charAt(@position)

	advance: (distance = 1) =>
		if @isAtEnd()
			throw "Already at the end of the atream, can't advnace()"

		newLineNumber = if @getCurrent() == '\n' then @line = 1 else @line
		newColumnNumber = if @getCurrent() == '\n' then 1 else @column + distance

		new Input(@source, @position + distance, newLineNumber, newColumnNumber)

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

	parser.Once = () ->
		parser.Select((r) -> [ r ])

	parser.AtLeastOnce = () ->
		parser.Once()
			.Then(
				(t1) ->
					parser.XMany().Select(
						(ts) -> t1.concat(ts)
					)
			)

	#Implemented imperatively to decrease stack usage.
	parser.Many = () ->
		Parser (i) ->
			remainder = i
			result = []
			r = parser(i)

			while (r.successful)
				if remainder.isEqual(r.remainder)
					break

				result.push(r.value)
				remainder = r.remainder
				r = parser(remainder)

			return Success(result, remainder)

	parser.XMany = () ->
		parser.Many().Then((m) -> parser.Once().XOr(Return(m)))	
		
	parser.XOr = (second) ->
		throw "no second parser in XOr()" if not second

		Parser (i) ->
			firstResult = parser(i)

			if not firstResult.successful
				#The 'X' part
				if not firstResult.remainder.isEqual(i)
					return firstResult

				secondResult = second(i)

				if not secondResult.successful
					return DetermineBestError firstResult, secondResult

				return secondResult

			#This handles a zero-length successful application of first.
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

	parser.Concat = (second) ->
		throw "no second parser in Concat()" if not second

		first = parser

		first.Then((f) -> second.Select((s) -> f.concat(s)))

	parser.Select = (map) ->
		throw "no map supplied to in Select()" if not map

		parser.Then (x) -> Parse.Return(map(x))

	parser.Text = () ->
		parser.Select((chars) -> chars.join(''))

	parser.Token = () ->
		Parse.WhiteSpace().Select () ->
			parser.Select (x) ->
				Parse.WhiteSpace().Select () ->
					return x;

	# names part of a grammar for better error messages
	parser.Named: (description) ->
		throw "no description supplied to Named()" if not description

		Parser (i) ->
			firstResult = parser(i)

			if not firstResult.successful
				if firstResult.remainder.isEqual(i)
					return Failure(firstResult.remainder, firstResult.message, [ description ])
				else
					return firstResult

			return firstResult

	return parser

Return = (value) ->
	Parser (i) -> Success(value, i)

Parse =
	Char: (charOrPredicate, description) ->
		throw "charOrPredicate missing" if not charOrPredicate
		throw "description missing" if not description

		#if they give us a character, turn that into a predicate
		if (typeof(charOrPredicate) != 'function')
			char = charOrPredicate
			charOrPredicate = (c) -> c == char

		Parser (i) -> 

			if not i.isAtEnd()
				
				if charOrPredicate(i.getCurrent())

					return Success(i.getCurrent(), i.advance())

				return Failure(i, "Unexpected #{i.getCurrent()}", [ description ])

			return Failure(i, "Unexpected end of input reached", [ description ])

	CharExcept: (charOrPredicate, description) ->
		throw "charOrPredicate missing" if not charOrPredicate
		throw "description missing" if not description

		#if they give us a character, turn that into a predicate
		if (typeof(charOrPredicate) != 'function')
			char = charOrPredicate
			charOrPredicate = (c) -> c == char

		return Parse.Char(
			(c) -> not charOrPredicate(c)
			description
		)

	String: (text, description) ->
		throw "text missing" if not text
		throw "description missing" if not description
		
		Parser (i) ->
			(Parse.Char for c in text.split(''))
				.reduce(
					(a, p) -> a.Concat(p.Once())
					Return []
				)
				.Named text


	WhiteSpace: () ->
		whitSpaceChars = [
			' '
			'\t'
			'\n'
			'\r'
		]

		Parse.Char(
			(c) -> whitSpaceChars.indexOf(c) >= 0
			'whitespace'
		)

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

semicolon = Parse.Char ';', 'a semicolon'

integer = Parse.Digit() # a digit (0, 1, 2, etc)
	.AtLeastOnce() 	#as many in a row as there are
	.Text() 	#combine into one string
	.Select(parseInt) #parse to a proper integer

grammar = integer

#should succeed
console.log '-------------Test 1, should succeed ------------------'
console.log JSON.stringify grammar.Parse("123;")

#should fail
console.log '-------------Test 2, should fail ------------------'
console.log Parse.WhiteSpace().Parse("    \t ")

#console.log Parse.Parse(grammar, stringToParse)
