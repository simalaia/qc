
-- generates generators...?

local Gen = {}
local Gen_mt = { __index = Gen }

-- creates new generator for generating random values
function Gen.new(pick, shrink)
  local Generator = {
    pick_func = pick,
    shrink_func = shrink,
    }
  return setmetatable(Generator, Gen_met)
end

-- Generates new random val based on this gen's pick val
function Gen:pick(numtests)
  return self.pick_func(numtests)
end

-- Shrinks generated val to simpler val
function Gen:shrink(prev)
  return self.shrink_func(prev)
end

return Gen
