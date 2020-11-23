defmodule IdC do
  defstruct [:id]
end

defmodule IfC do
  defstruct [:test, :then, :else]
end

defmodule AppC do
	defstruct [:body, :args]
end

defmodule LamC do
  defstruct [:body, :params]
end

defmodule NumV do
	@enforce_keys [:val]
	defstruct [:val]
end

defmodule StrV do
	@enforce_keys [:val]
	defstruct [:val]
end

defmodule BoolV do
	@enforce_keys [:val]
	defstruct [:val]
end

defmodule PrimV do
	@enforce_keys [:func]
	defstruct [:func]
end

defmodule CloV do
	@enforce_keys [:body, :params, :clo_env]
	defstruct [:body, :params, :clo_env]
end

defmodule M do

	def interp(expr, env) do
		cond do
			(expr.__struct__ == NumV) -> expr
			(expr.__struct__ == StrV) -> expr
			(expr.__struct__ == BoolV) -> expr
			(expr.__struct__ == PrimV) -> 1
			(expr.__struct__ == CloV)-> 1
			(expr.__struct__ == IdC) -> lookup_env(env, expr.id)
			(expr.__struct__ == AppC) ->
				interpretedBody = interp(expr.body, env)
				cond do
					length(expr.args) > 2 -> "ERROR: invalid number of arguments"
					true ->
						[firstEl|rest] = expr.args
						second = List.first(rest)
						a = interp(firstEl, env)
						b = interp(second, env)
						#IO.puts interpretedBody.(a, b).val
						interpretedBody.(a, b)
				end
			(expr.__struct__ == CondC) -> 1
			(expr.__struct__ == LamC) -> 1
			true -> "ERROR"
		end
	end

	def init_interp(ast) do
		bindTrue = {:true, true}
		bindFalse = {:false, false}
		bindAdd = {:+,  fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val + b.val)}
												true -> "ERROR: invalid operands for +"
											end
										end}
		bindSub = {:-, fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val - b.val)}
												true -> "ERROR: invalid operands for -"
										    end
										end}
		bindMult = {:*, fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val * b.val)}
												true -> "ERROR: invalid operands for *"
											end
										end}
		bindDiv = {:/, fn (a, b) ->
											cond do
												(b.__struct__ == NumV && b.val == 0) -> "ERROR: dividing by 0"
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val / b.val)}
												true -> "ERROR: invalid operands for /"
											end
										end}
		bindLeq = {:leq, fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %BoolV{val: (a.val <= b.val)}
												true -> "ERROR: invalid operands for leq"
											end
										end}
		bindEqual = {:equal?, fn (a, b) ->
											cond do
												((a.__struct__ != PrimV || a.__struct__ != CloV) && (b.__struct__ != PrimV || b.__struct__ != CloV)) ->
													%BoolV{val: (a.val == b.val)}
												true -> %BoolV{val: false}
											end
										end}
		bindError = {:error, fn (a) -> "ERROR: #{a}" end}
		top_env = [bindTrue, bindFalse, bindAdd, bindSub, bindMult, bindDiv, bindLeq, bindEqual, bindError]

		# IO.puts lookup_env(top_env, :+)

		interp(ast, top_env)
	end

	def lookup_env(env, target) do
		[first|rest] = env
		cond do
			length(env) == 0 -> "ERROR: symbol not found"
			elem(first, 0) == target -> elem(first, 1)
			true -> lookup_env(rest, target)
		end
	end

	def serialize(expr) do
		cond do
			(expr.__struct__ == NumV) -> IO.puts expr.val
			(expr.__struct__ == StrV) -> IO.puts expr.val
			(expr.__struct__ == BoolV) -> cond do
											(expr.val) -> IO.puts "True"
											true -> IO.puts "False"
										end
			(expr.__struct__ == PrimV) -> IO.puts "#<primop>"
			(expr.__struct__ == CloV) -> IO.puts "#<procedure>"
		end
  end

  def parse(l) do
    #IO.puts(l)
    keywords = [:let, :in, :if, :fn]
    cond do
      is_integer(l) -> %NumV{val: l}
      is_atom(l) -> cond do
        Enum.member?(keywords, l) -> raise "Unable to use keyword as id"
        true -> %IdC{id: l}
      end
      is_binary(l) -> %StrV{val: l}
      is_list(l) -> [first | rest] = l

      cond do
        length(l) == 1 -> %AppC{body: parse(first), args: []}
        true -> cond do
          first == :if -> %IfC{test: parse(Enum.at(rest, 0)), then: parse(Enum.at(rest, 1)), else: parse(Enum.at(rest, 2))}
          first == :fn -> %LamC{body: parse(Enum.at(rest, 1)), params: Enum.map(Enum.at(rest, 0), fn (arg) -> parse(arg) end)}
          first == :let && Enum.at(rest, length(rest) - 2) == :in -> parse_let(l)
          true -> %AppC{body: parse(first), args: Enum.map(rest, fn (arg) -> parse(arg) end)}
        end
      end
      true -> raise "invalid syntax error"
    end
  end

  def parse_let(l) do
    test = [:let, [:x, := ,1], [:y, :=, 2], :in, [:+, :x, :y]]
    body = Enum.at(l, length(l) - 1)
    args = Enum.map(Enum.slice(l, 1..length(l)-3), fn (arg) -> Enum.at(arg, 0) end)
    vals = Enum.map(Enum.slice(l, 1..length(l)-3), fn (arg) -> Enum.at(arg, 2) end)
    %AppC{body: parse([:fn, args, body]), args: Enum.map(vals, fn (arg) -> parse(arg) end)}

  end

#	def top_interp(input) do
#		serialize(interp(parse(input), top_env))
#	end

	def main() do
		testId = %IdC{id: :equal?}
		a = %NumV{val: 1}
		b = %NumV{val: 3}
		testAppC = %AppC{body: testId, args: [a, b]}
		init_interp(testAppC)
    init_interp(%AppC{body: %IdC{id: :*}, args: [a, b]})
    testParse = parse(1)
    #IO.puts(testParse.val)
    test = [:let, [:x, := ,1], [:y, :=, 2], :in, [:+, :x, :y]]
    testParse = parse(test)
    t = testParse.body.params
    IO.puts(Enum.at(t, 0).id)
	end

end


M.main()
ExUnit.start()
defmodule TestCases do
  use ExUnit.Case
  use ExUnit.Case, async: true
  import M
  test "the truth" do
    assert 1 + 1 == 2
  end
  test "add nums" do
    testId = %IdC{id: :+}
		a = %NumV{val: 1}
		b = %NumV{val: 1}
    testAppC = %AppC{body: testId, args: [a, b]}
    assert init_interp(testAppC).val == 2
  end
  test "sub nums" do
    testId = %IdC{id: :-}
		a = %NumV{val: 1}
		b = %NumV{val: 1}
    testAppC = %AppC{body: testId, args: [a, b]}
    assert init_interp(testAppC).val == 0
  end
  test "mult nums" do
    testId = %IdC{id: :*}
		a = %NumV{val: 1}
		b = %NumV{val: 1}
    testAppC = %AppC{body: testId, args: [a, b]}
    assert init_interp(testAppC).val == 1
  end
  test "div nums" do
    testId = %IdC{id: :/}
		a = %NumV{val: 1}
		b = %NumV{val: 1}
    testAppC = %AppC{body: testId, args: [a, b]}
    assert init_interp(testAppC).val == 1
  end
  test "equal vals" do
    testId = %IdC{id: :equal?}
		a = %NumV{val: 1}
		b = %NumV{val: 1}
    testAppC = %AppC{body: testId, args: [a, b]}
    assert init_interp(testAppC).val == true
  end
  test "leq vals" do
    testId = %IdC{id: :leq}
		a = %NumV{val: 1}
		b = %NumV{val: 1}
    testAppC = %AppC{body: testId, args: [a, b]}
    assert init_interp(testAppC).val == true
  end
  #Serialize tests
  #test "serialize num" do
	#assert serialize(%NumV{val: 2}) == "2"
  #end
end
