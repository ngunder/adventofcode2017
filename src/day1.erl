%%%-------------------------------------------------------------------
%%% @author Nicholas Gunder
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 01. Dec 2017 17.22

%%%-------------------------------------------------------------------
-module(day1).
-author("ngunder").

%%--- Day 1: Inverse Captcha ---
%%
%% The night before Christmas, one of Santa's Elves calls you in a panic. "The printer's broken! We can't print the
%% Naughty or Nice List!" By the time you make it to sub-basement 17, there are only a few minutes until midnight.
%% "We have a big problem," she says; "there must be almost fifty bugs in this system, but nothing else can print The
%% List. Stand in this square, quick! There's no time to explain; if you can convince them to pay you in stars, you'll
%% be able to--" She pulls a lever and the world goes blurry.
%%
%% When your eyes can focus again, everything seems a lot more pixelated than before. She must have sent you inside
%% the computer! You check the system clock: 25 milliseconds until midnight. With that much time, you should be able
%% to collect all fifty stars by December 25th.
%%
%% Collect stars by solving puzzles. Two puzzles will be made available on each day millisecond in the advent calendar;
%% the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
%%
%% You're standing in a room with "digitization quarantine" written in LEDs along one wall. The only door is locked,
%% but it includes a small interface. "Restricted Area - Strictly No Digitized Users Allowed."
%%
%% It goes on to explain that you may only leave by solving a captcha to prove you're not a human. Apparently, you only
%% get one millisecond to solve the captcha: too fast for a normal human, but it feels like hours to you.
%%
%% The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that
%% match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the
%% list.
%%
%% For example:
%%
%% 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2)
%% matches the fourth digit.
%% 1111 produces 4 because each digit (all 1) matches the next.
%% 1234 produces 0 because no digit matches the next.
%% 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.
%% What is the solution to your captcha?
%%
%% Your puzzle answer was 1097.
%%
%% The first half of this puzzle is complete! It provides one gold star: *
%%
%% --- Part Two ---
%%
%% You notice a progress bar that jumps to 50% completion. Apparently, the door isn't yet satisfied, but it did emit a
%% star as encouragement. The instructions change:
%%
%% Now, instead of considering the next digit, it wants you to consider the digit halfway around the circular list.
%% That is, if your list contains 10 items, only include a digit in your sum if the digit 10/2 = 5 steps
%% forward matches it. Fortunately, your list has an even number of elements.
%%
%% For example:
%%
%% 1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
%% 1221 produces 0, because every comparison is between a 1 and a 2.
%% 123425 produces 4, because both 2s match each other, but no other digit has a match.
%% 123123 produces 12.
%% 12131415 produces 4.
%% What is the solution to your new captcha?

-define(INPUT, "818275977931166178424892653779931342156567268946849597948944469863818248114327524824136924486891794739281668741616818614613222585132742386168687517939432911753846817997473555693821316918473474459788714917665794336753628836231159578734813485687247273288926216976992516314415836985611354682821892793983922755395577592859959966574329787693934242233159947846757279523939217844194346599494858459582798326799512571365294673978955928416955127211624234143497546729348687844317864243859238665326784414349618985832259224761857371389133635711819476969854584123589566163491796442167815899539788237118339218699137497532932492226948892362554937381497389469981346971998271644362944839883953967698665427314592438958181697639594631142991156327257413186621923369632466918836951277519421695264986942261781256412377711245825379412978876134267384793694756732246799739464721215446477972737883445615664755923441441781128933369585655925615257548499628878242122434979197969569971961379367756499884537433839217835728263798431874654317137955175565253555735968376115749641527957935691487965161211853476747758982854811367422656321836839326818976668191525884763294465366151349347633968321457954152621175837754723675485348339261288195865348545793575843874731785852718281311481217515834822185477982342271937155479432673815629144664144538221768992733498856934255518875381672342521819499939835919827166318715849161715775427981485233467222586764392783699273452228728667175488552924399518855743923659815483988899924199449721321589476864161778841352853573584489497263216627369841455165476954483715112127465311353411346132671561568444626828453687183385215975319858714144975174516356117245993696521941589168394574287785233685284294357548156487538175462176268162852746996633977948755296869616778577327951858348313582783675149343562362974553976147259225311183729415381527435926224781181987111454447371894645359797229493458443522549386769845742557644349554641538488252581267341635761715674381775778868374988451463624332123361576518411234438681171864923916896987836734129295354684962897616358722633724198278552339794629939574841672355699222747886785616814449297817352118452284785694551841431869545321438468118").

%% API
-export([part_a/0,
         part_b/0,
         a/1,
         b/1,
         test/0]).

part_a() ->
  a(?INPUT).
part_b() ->
  b(?INPUT).

%% PART A:
a([First|_] = Input) ->
  a(Input, 0, [First]).

a([], Sum, _) ->
  Sum;
a(Last, Sum, Last) ->
  Sum+list_to_integer(Last);
a([A, A|Rest], Sum, First) ->
  a([A|Rest], Sum+list_to_integer([A]), First);
a([_, B|Rest], Sum, First) ->
  a([B|Rest], Sum, First);
a(_, Sum, _) ->
  Sum.

%% PART B
b(In) ->
  b(lists:split(length(In) div 2, In), 0).

b({[], []}, Sum) ->
  Sum*2;
b({[A|Rest1], [A|Rest2]}, Sum) ->
  b({Rest1, Rest2}, Sum+list_to_integer([A]));
b({[_A|Rest1], [_B|Rest2]}, Sum) ->
  b({Rest1, Rest2}, Sum).

test()->
  pass=test_part_a(),
  pass=test_part_b().

test_part_a() ->
  4 = a("1111"),
  3 = a("1122"),
  0 = a("1234"),
  9 = a("91212129"),
  pass.
test_part_b() ->
  6 = b("1212"),
  0 = b("1221"),
  4 = b("123425"),
  12 = b("123123"),
  4 = b("12131415"),
  pass.