local obj = {}
obj.__index = obj
local log = hs.logger.new("yabai")
function obj:run(args, completion)
	local rargs = hs.inspect.inspect(args)
	local yabai_output = ""
	local yabai_error = ""
	-- Runs in background very fast
	local yabai_task = hs.task.new("/run/current-system/sw/bin/yabai", function(err, stdout, stderr)
		if err ~= 0 then
			log.e(rargs, err, stderr, stdout)
		else
			log.d(rargs, err, stderr, stdout)
		end
	end, function(task, stdout, stderr)
		if stdout ~= nil then
			yabai_output = yabai_output .. stdout
		end
		if stderr ~= nil then
			yabai_error = yabai_error .. stderr
		end
		return true
	end, args)
	if type(completion) == "function" then
		yabai_task:setCallback(function()
			completion(yabai_output, yabai_error)
		end)
	end
	yabai_task:start()
end
return obj
