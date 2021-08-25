# Register a new lexer for Fu
Jekyll::Hooks.register :site, :pre_render do |site|
  puts "Registering Fu highlighter"
  require "rouge"

  class FuLexer < Rouge::RegexLexer
	title 'Fu'
	aliases 'fu'
    tag 'fu'
    filenames '*.fu'
    desc 'The Fu Language'

	KEYWORDS = %w(
      Nat Mem Bool Byte Unit Int Word Float
      Int8 Int16 Int32 Int64
      Word8 Word16 Word32 Word64
      Float16 Float32 Float64
      Binary16 Binary32 Binary64
      type struct enum mod sig ref control filter
      match if else for while in let val var attr fun)
	KEYWORD_CONSTANTS = %w(true false)
    state :root do
        rule %r/#.*$/, Comment::Single
        rule %r/\s+/m, Text::Whitespace

        rule %r/'.+'/m, Str::Single
        rule %r/".+"/m, Str::Double
        rule %r/\.\.\./m, Comment::Special
        rule %r/\.\./m, Operator

        rule %r/0[xX][a-fA-F0-9]+([pP][0-9]+)?[Li]?/, Num::Hex
        rule %r/[+-]?(\d+([.]\d+)?|[.]\d+)([eE][+-]?\d+)?[Li]?/, Num

        rule %r/([[:alpha:]]|_)(_|[[:alnum:]])*/ do |m|
          if KEYWORDS.include? m[0]
            token Keyword
          elsif KEYWORD_CONSTANTS.include? m[0]
            token Keyword::Constant
          else
            token Name
          end
        end

        rule %r/[\[\]{}();,]/, Punctuation

        rule %r([-<>?*+^/!=~$@:%&|\.]), Operator
      end
  end
end
