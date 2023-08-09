-- Functions
-- -----------------------------------------------

function pastePlainText()
   p = hs.pasteboard.readDataForUTI(nil, "public.utf8-plain-text")
   hs.pasteboard.setContents(p)
   app = hs.application.frontmostApplication()
   app:selectMenuItem({"Edit", "Paste"})
end

function newMailMessage() os.execute("open mailto:") end

function snipWave() hs.eventtap.keyStrokes("(waving hands around)") end
function snipShrug() hs.eventtap.keyStrokes(" ¯\\_(ツ)_/¯") end
function snipOrgDate() hs.eventtap.keyStrokes(os.date("<%Y-%m-%d %a>")) end
function snipISODate() hs.eventtap.keyStrokes(os.date("%Y-%m-%d")) end

function menuCapitalize()
   local app = hs.application.frontmostApplication()
   app:selectMenuItem({"Edit", "Transformations", "Capitalize"})
end

function menuUpperCase()
   local app = hs.application.frontmostApplication()
   app:selectMenuItem({"Edit", "Transformations", "Make Upper Case"})
end

function menuLowerCase()
   local app = hs.application.frontmostApplication()
   app:selectMenuItem({"Edit", "Transformations", "Make Lower Case"})
end

function webSearch(name, url)
   hs.focus()
   button, message = hs.dialog.textPrompt(name, "Search " .. name .. "for:", "", "Search", "Cancel")
   if button == 'Cancel' then
	  return
   else
	  search = hs.http.encodeForQuery(message)
	  hs.urlevent.openURL(url .. search)
   end
end

function searchYouTube()   webSearch("YouTube",   "http://www.youtube.com/results?search_query=") end
function searchGitHub()    webSearch("GitHub",    "https://github.com/search?q=") end
function searchWikipedia() webSearch("Wikipedia", "https://en.wikipedia.org/w/index.php?search=") end
function searchIMDB()      webSearch("IMDB",      "https://www.imdb.com/find?q=") end
function searchWolframAlpha() webSearch("Wolfram Alpha", "https://www.wolframalpha.com/input?i=") end

function searchGoogle()
   -- This function was written by chatGTP
   -- Create a chooser for user input
   local chooser = hs.chooser.new(function(choice)
	  if not choice then return end
	  -- Open a URL for a Google search with the selected query
	  hs.urlevent.openURL("https://www.google.com/search?q=" .. hs.http.encodeForQuery(choice.text))
	  -- hs.alert(hs.http.encodeForQuery(choice.text))
   end)
   chooser:placeholderText("Google Search...")
   -- Fetch autocomplete suggestions from Google
   chooser:queryChangedCallback(function(string)
	  if #string == 0 then return end
	  hs.http.asyncGet("https://suggestqueries.google.com/complete/search?client=firefox&q=" .. hs.http.encodeForQuery(string), nil, function(status, body)
		 if status ~= 200 then return end
		 local suggestions = hs.json.decode(body)[2]
		 local results = {}
		 for i, suggestion in ipairs(suggestions) do
			table.insert(results, {
			   ["text"] = suggestion
			})
		 end
		 chooser:choices(results)
	  end)
   end)
   chooser:show()
end

function newFinderWindow()
   finder = hs.appfinder.appFromName("Finder")
   hs.osascript.applescript('tell application "Finder" to make new Finder window')
   finder:activate()
end
