defmodule NumC do
  defstruct [:num]
end

defmodule IdC do
  defstruct [:id]
end

defmodule StrC do
  defstruct [:str]
end

defmodule IfC do
  defstruct [:test, :then, :else]
end

defmodule LamC do
  defstruct [:args, :body]
end

defmodule AppC do
  defstruct [:func, :args]
end

defmodule IfC do
  defstruct [:test, :then, :else]
end

defmodule NumV do
  defstruct [:val]
end

defmodule StrV do
  defstruct [:str]
end

defmodule BoolV do
  defstruct [:val]
end

defmodule PrimV do
  defstruct [:op]
end

defmodule ClosV do
  defstruct [:args, :body, :env]
end

defmodule M do

	def interp(expr, env) do
		cond do
			(expr.__struct__ == NumC) -> %NumV{val: expr.num}
			(expr.__struct__ == StrC) -> %StrV{str: expr.str}
			(expr.__struct__ == IdC) -> lookup_env(env, expr.id)
      (expr.__struct__ == AppC) ->
				interpretedBody = interp(expr.func, env)
				cond do
					length(expr.args) > 2 -> "ERROR: invalid number of arguments"
					true ->
						[firstEl|rest] = expr.args
						second = List.first(rest)
						a = interp(firstEl, env)
						b = interp(second, env)
						#IO.puts interpretedBody.(a, b).val
						interpretedBody.(a, b).val
				end
			(expr.__struct__ == CondC) -> IO.puts("E")
			(expr.__struct__ == LamC) -> IO.puts("F")
			true -> "ERROR"
		end
	end

	def init_interp(ast) do
		bindAdd = {:+,  fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val + b.val)}
												true -> "ERROR: invalid operands for +"
											end
										end}
		bindSub = {:-, fn (a, b) ->
                      cond do
                        (a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val - b.val)}
                        true -> "ERROR: invalid operands for +"
                      end
                    end}
		bindMult = {:*, fn (a, b) ->
                      cond do
                        (a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val * b.val)}
                        true -> "ERROR: invalid operands for +"
                      end
                    end}
		bindDiv = {:/, fn (a, b) ->
                      cond do
                        (b.__struct__ == NumV && b.val == 0) -> raise "ERROR: dividing by 0"
                        (a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val / b.val)}
                        true -> "ERROR: invalid operands for /"
                      end
                    end}
		bindLeq = {:<=, fn (a, b) ->
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
		top_env = [bindAdd, bindSub, bindMult, bindDiv, bindLeq, bindEqual]

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

  #def parse(l) do
    #IO.puts(l)
  #  cond do
  #    length(l) == 1 -> [first] = l
  #    cond do
  #      is_integer(first) -> %NumC{num: first}
  #      is_atom(first) -> %IdC{id: first}
  #      true -> %StrC{str: first}
  #    end
  ##    length(l) > 1 -> [first | rest] = l
  #    cond do
  #      first == "if" -> %IfC{test: parse([Enum.at(rest, 0)]), then: parse([Enum.at(rest, 1)]), else: parse([Enum.at(rest, 2)])}
  #      first == "fn" -> %LamC{args: Enum.map(Enum.at(rest, 0), fn (arg) -> parse([arg]) end), body: parse([Enum.at(rest, 1)])}

  #    end
  #  end
  #end


  ##parses a value or list of values into an ExprC
  def parse(l) do
    keywords = [:let, :in, :if, :fn]
    cond do
      is_integer(l) -> %NumC{num: l}
      is_atom(l) -> cond do
        Enum.member?(keywords, l) -> raise "Unable to use keyword as id"
        true -> %IdC{id: l}
      end
      is_binary(l) -> %StrC{str: l}
      is_list(l) -> [first | rest] = l

      cond do
        length(l) == 1 -> %AppC{func: parse(first), args: []}
        true -> cond do
          first == :if -> %IfC{test: parse(Enum.at(rest, 0)), then: parse(Enum.at(rest, 1)), else: parse(Enum.at(rest, 2))}
          first == :fn -> %LamC{args: Enum.map(Enum.at(rest, 0), fn (arg) -> parse(arg) end), body: parse(Enum.at(rest, 1))}
          first == :let && Enum.at(rest, length(rest) - 2) == :in -> parse_let(l)
          true -> %AppC{func: parse(first), args: Enum.map(rest, fn (arg) -> parse(arg) end)}
        end
      end
    end
  end

  ##helper function for parsing let statement into AppC
  def parse_let(l) do
    #test = [:let, [:x, := ,1], [:y, :=, 2], :in, [:+, :x, :y]]
    body = Enum.at(l, length(l) - 1)
    args = Enum.map(Enum.slice(l, 1..length(l)-3), fn (arg) -> Enum.at(arg, 0) end)
    vals = Enum.map(Enum.slice(l, 1..length(l)-3), fn (arg) -> Enum.at(arg, 2) end)
    %AppC{func: parse2([:fn, args, body]), args: vals}
  end

	def main() do
		#testId = %IdC{id: :/}
		#a = %NumC{num: 1}
		#b = %NumC{num: 1}
		#testAppC = %AppC{func: testId, args: [a, b]}
    #init_interp(testAppC)
    #extend_env([1,2], [5, 6], [3, 4])
    #test = parse(["fn", [:a, :b], 1])
    #l = ["if", 0, 1, 2]
    #test2 = parse2([:f, 1, 2])
    #args = test.args
    #IO.puts(Enum.at(args, 0).id)
    #IO.puts(test2)
    #args = test2.args
    #IO.puts(Enum.at(args, 0).num)
    test = [:let, [:x, := ,1], :in, [:+, :x, 1]]
    testParse = parse2(test)
    #t = testParse.args
    #IO.puts(Enum.at(t, 1))
    IO.inspect(testParse)
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
		a = %NumC{num: 1}
		b = %NumC{num: 1}
    testAppC = %AppC{func: testId, args: [a, b]}
    assert init_interp(testAppC) == 2
  end
  test "sub nums" do
    testId = %IdC{id: :-}
		a = %NumC{num: 1}
		b = %NumC{num: 1}
    testAppC = %AppC{func: testId, args: [a, b]}
    assert init_interp(testAppC) == 0
  end
  test "mult nums" do
    testId = %IdC{id: :*}
		a = %NumC{num: 1}
		b = %NumC{num: 1}
    testAppC = %AppC{func: testId, args: [a, b]}
    assert init_interp(testAppC) == 1
  end
  test "div nums" do
    testId = %IdC{id: :/}
		a = %NumC{num: 1}
		b = %NumC{num: 1}
    testAppC = %AppC{func: testId, args: [a, b]}
    assert init_interp(testAppC) == 1
  end
  test "div by 0" do
    testId = %IdC{id: :/}
		a = %NumC{num: 1}
		b = %NumC{num: 0}
    testAppC = %AppC{func: testId, args: [a, b]}
    catch_error(init_interp(testAppC))
  end
  test "equal vals" do
    testId = %IdC{id: :equal?}
		a = %NumC{num: 1}
		b = %NumC{num: 1}
    testAppC = %AppC{func: testId, args: [a, b]}
    assert init_interp(testAppC) == true
  end
  test "leq vals" do
    testId = %IdC{id: :<=}
		a = %NumC{num: 1}
		b = %NumC{num: 1}
    testAppC = %AppC{func: testId, args: [a, b]}
    assert init_interp(testAppC) == true
  end
end
#defmodule Main do
#  def main do
#    test = %StrC{str: "Hello"}
#    IO.puts(test.str)
#  end
#end

#Main.main
