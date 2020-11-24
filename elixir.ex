defmodule IdC do
  defstruct [:id]
end

defmodule AppC do
	defstruct [:body, :args]
end

defmodule LamC do
  defstruct [:body, :params]
end

defmodule CondC do
	defstruct [:if, :then, :else]
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
			(expr.__struct__ == IdC) -> lookup_env(env, expr.id)
			(expr.__struct__ == AppC) ->
				interpretedBody = interp(expr.body, env)
				cond do
					interpretedBody.__struct__ == PrimV ->
						cond do
							length(expr.args) > 2 -> "ERROR: invalid number of arguments"
							true ->
								[firstEl|rest] = expr.args
								second = List.first(rest)
								a = interp(firstEl, env)
								b = interp(second, env)
								IO.puts interpretedBody.func.(a, b).val
								# interpretedBody.(a, b)
						end
					interpretedBody.__struct__ == CloV ->
						newIds = interpretedBody.params
						interpretedArgs = Enum.map(expr.args, fn (arg) -> interp(arg, env) end)
						newEnv = extendEnv(newIds, interpretedArgs, env)
						interp(interpretedBody.body, newEnv)
					true -> "ERROR: applied arguments to non-function"
				end
			(expr.__struct__ == CondC) -> interpCond(interp(expr.if), expr.then, expr.else)
			(expr.__struct__ == LamC) -> %CloV{body: expr.body, params: expr.params, clo_env: env}
			true -> "ERROR"
		end
	end


	def interpCond(boolean, ifTrue, ifElse) do
		cond do
			boolean.__struct__ == BoolV ->
				cond do
					boolean.val -> interp(ifTrue)
					true -> interp(ifElse)
				end
			true -> "ERROR: if statement is not a boolean"
		end
	end


	def extendEnv(ids, args, oldEnv) do
		cond do
			(length(ids) == 0 and !(length(args) == 0)) or (!(length(ids) == 0) and length(args) == 0) -> "ERROR: uneven numbers of ids and args"
			length(ids) == 0 -> oldEnv
			true ->
				[firstId|restIds] = ids
				[firstArg|restArgs] = args
				newEnv = List.insert_at(oldEnv, 0, {firstId, firstArg})
				IO.puts firstId
				IO.puts firstArg
				extendEnv(restIds, restArgs, newEnv)
		end
	end


	def testExtendEnv() do
		ids = [:a, :b, :c]
		args = [1, 2, 3]
		newEnv = extendEnv(ids, args, [])
		# IO.puts List.first(newEnv)
	end


	def init_interp(ast) do
		bindAdd = {:+,  %PrimV{func: fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val + b.val)}
												true -> "ERROR: invalid operands for +"
											end
										end}}
		bindSub = {:-, %PrimV{func: fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val - b.val)}
												true -> "ERROR: invalid operands for -"
										    end
										end}}
		bindMult = {:*, %PrimV{func: fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val * b.val)}
												true -> "ERROR: invalid operands for *"
											end
										end}}
		bindDiv = {:/, %PrimV{func: fn (a, b) ->
											cond do
												(b.__struct__ == NumV && b.val == 0) -> "ERROR: dividing by 0"
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val / b.val)}
												true -> "ERROR: invalid operands for /"
											end
										end}}
		bindLeq = {:leq, %PrimV{func: fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %BoolV{val: (a.val <= b.val)}
												true -> "ERROR: invalid operands for leq"
											end
										end}}
		bindEqual = {:equal?, %PrimV{func: fn (a, b) ->
											cond do
												((a.__struct__ != PrimV || a.__struct__ != CloV) && (b.__struct__ != PrimV || b.__struct__ != CloV)) ->
													%BoolV{val: (a.val == b.val)}
												true -> %BoolV{val: false}
											end
										end}}
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


	def main() do
		testId = %IdC{id: :/}
		a = %NumV{val: 4}
		b = %NumV{val: 2}
		#testAppC = %AppC{body: testId, args: [a, b]}
		#init_interp(testAppC)
	end

end


M.main()
