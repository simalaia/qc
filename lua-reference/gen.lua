
local Gen = require("generator")
local random = require("random")
local reduce = require("helpers.reduce")

local lib = {}

-- picks number ranomly between min/max
local function choose_pick(min, max)
  local function pick()
    return random.between(min, max)
  end
  return pick
end


-- shrinks val between min/max by dividing the sum
-- of the closest number to 0 and the generated value
-- by two effectively reducing it to value closest
-- to 0 gradually in range
local function choose_shrink(min, max)
  local shrink_to = (math.abs(min) < math.abs(max)) and min or max
  local function shrink(value)
    local shrunk_value = (shrink_to + value) / 2

    if shrunk_value < 0 then return math.ceil(shrunk_value)
    else return math.floor(shrunk_value) end
  end

  return shrink
end

-- creates generator, choose between min/max inclusive
function lib.choose(min, max)
  return Gen.new(choose_pick(min,max), choose_shrink(min,max))
end

-- select generator from list of generators
function lib.oneof(generators)
  local which -- shared state between pick/shrink needed to shrink
  local function oneof_pick(numtests)
    which = random.between(1, #generators)
    return generators[which]:pick(numtests)
  end
  local function oneof_shrink(prev)
    return generators[which]:shrink(prev)
  end

  return Gen.new(oneof_pick, oneof_shrink)
end

-- select generator from list of weighted generators
function lib.frequency(generators)
  local which
  local function do_sum(generator, acc) return generator[1] + acc end
  local function frequency_pick(numtests)
    local sum = reduce(generators, 0, do_sum)
    local val = random.between(1, sum)
    which = reduce(generators, {0, 1}, function(generator, acc)
      local current_sum = acc[1] + generator[1]
      if current_sum >= val then return acc
      else return { current_sum, acc[2] + 1 } end
    end)[2]

    return generators[which][2]:pick(numtests)
  end
  local function frequency_shrink(prev)
    return generators[which][2]:shrink(prev)
  end

  return Gen.new(frequency_pick, frequency_shrink)
end

-- Create generator that selects element based on input list
function lib.elements(array)
  local last_idx
  local function elements_pick()
    local idx = random.between(1, #array)
    last_idx = idx
    return array[idx]
  end

  local function elements_shrink(_)
    if last_idx > 1 then last_idx = last_idx - 1 end
    return array[last_idx]
  end

  return Gen.new(elements_pick, elements_shrink)
end

return lib
