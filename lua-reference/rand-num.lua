
-- @module lqc.random
-- @alias lib
local time = os.time
local random_seed = math.randomseed
local random = math.random

local lib = {}

-- Seed random generator
function lib.seed(seed)
  if not seed then seed = time() end
  random_seed(seed)
  return seed
end

-- Random number between min/max
function lib.between(min, max)
  return random(min, max)
end

return lib
