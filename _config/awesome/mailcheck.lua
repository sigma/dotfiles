-- Awesome 3.x Mailchecker Widget
-- Checks for new mail in a Maildir mail folder
--
-- 0.1:
-- initial release
--

-- Lua filesystem library
-- (http://www.keplerproject.org/luafilesystem/index.html)
local lfs = require("lfs")

local assert = assert

module("mailcheck")

local mailbox
local widget
local background_color_empty
local background_color_newmail

--- Register Mailbox to a widget
-- @param w The widget to register, must be a textbox widget
-- @param mb Full path to Maildir folder
-- @param bge Background color to use if mailbox is empty (optional, defaults to black)
-- @param bgn Background color to use if mailbox is not empty (optional, defaults to darkgreen)
function register(w,mb,bge,bgn)
   local mode

   widget = w
   mailbox = mb
   if bge ~= nil then background_color_empty = bge else background_color_empty = "black" end
   if bgn ~= nil then background_color_newmail = bgn else background_Color_newmail = "darkgreen" end

   mode = lfs.attributes( mailbox .. "/new", "mode" )
   assert( mode == "directory", mailbox .. " does not look like a Maildir folder" )

   widget.bg = background_color_empty

end

--- Check for new messages
-- @return messagecount Number of messages in Mailfolder
function check()
   local messagecount = -2

   for messagefile in lfs.dir( mailbox .. "/new") do
      messagecount = messagecount + 1
   end

   if messagecount ~= 0 then
      widget.bg = background_color_newmail
   else
      widget.bg = background_color_empty
   end

   return { messagecount }
end
