
-- reduces array of values to single value

-- performs reduce operation
local function do_reduce(array, acc, func, pos)
  if pos < #array then
    local new_pos = pos + 1
    return do_reduce(array, func(array[new_pos], acc), func, new_pos)
  end
  return acc
end

-- reduces array of values into single value
local function reduce(array, start, func)
  return do_reduce(array, start, func, 0)
end

return reduce
