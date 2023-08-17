PacletInstall["Wolfram/LLMFunctions"]
Needs["Wolfram`LLMFunctions`"]

openaiModel = "gpt-4"

getLeftNumbers[thought_String] := 
 First@StringCases[thought, "(left: " ~~ x__ ~~ ")" :> x]

getValueHelper[evaluatedT_] := If[StringQ[evaluatedT],
   Switch[ToLowerCase[Last[StringSplit[evaluatedT, "\n"]]], "likely", 
    1, "sure", 20, "impossible", 0.1, _, 0],
   0
   ];

getCurrentNumbers[y_String] :=
 Module[{lastLine, numbers},
  lastLine = Last@StringSplit[y, "\n"];
  numbers = StringCases[lastLine, "left: " ~~ x__ ~~ ")" :> x];
  If[numbers == {}, lastLine, First[numbers]]
  ];

(*given a proposal (list of steps so far) return a list of proposals \
for the next step *)
getProposals [proposal_String, taskInput_String] :=
 Module[{proposals, currentNumbers, output},
  currentNumbers = 
   getCurrentNumbers[If[proposal != "", proposal, taskInput]];
  
  If[currentNumbers == "24",
   proposals = 
    generateAnswer[<|"input" -> taskInput, "steps" -> proposal|>],
   proposals = generateProposals[<|"input" -> currentNumbers|>]
   ];
  
  If[ListQ[proposals],
   output = (proposal <> "\n" <> #) & /@ proposals,
   output = (proposal <> "\n" <> #) &@proposals
   ];
  output
  ];

(* given a proposed step, evaluate the correct prompt (final step or \
normal step) nEval times and sum values*)
getValue[proposal_String, taskInput_String] :=
 Module[{thoughts, value, isLastStep, nEval = 3},
  If[StringFreeQ[proposal, "(left: " ~~ __ ~~ ")"],
   isLastStep = True,
   isLastStep = False
   ];
  
  If[TrueQ[isLastStep],
   thoughts = 
    Table[evaluateAnswer[<|"input" -> taskInput, 
       "answer" -> proposal|>], {nEval}],
   thoughts = 
    Table[evaluateProposal[<|
       "input" -> getLeftNumbers[proposal]|>], {nEval}]
   ];
  
  value = Total[getValueHelper /@ thoughts];
  value
  ];

(* given a list of proposals return a list of values*)
getValues[proposals_List, taskInput_String] :=
  Module[{proposal, values = {}, localValueCache = <||>, value},
   Do[
    If[KeyExistsQ[localValueCache, proposal],
     value = 0,
     value = getValue[proposal, taskInput];
     AssociateTo[localValueCache, proposal -> value]
     ];
    AppendTo[values, value],
    {proposal, proposals}
    ];
   values
   ];

llmParams = <|"Model" -> "gpt-4", "Temperature" -> 0.7|>;

generateProposals = LLMFunction["Input: 2 8 8 14
Possible next steps:
2 + 8 = 10 (left: 8 10 14)
8 / 2 = 4 (left: 4 8 14)
14 + 2 = 16 (left: 8 8 16)
2 * 8 = 16 (left: 8 14 16)
8 - 2 = 6 (left: 6 8 14)
14 - 8 = 6 (left: 2 6 8)
14 /  2 = 7 (left: 7 8 8)
14 - 2 = 12 (left: 8 8 12)
Input: `input`
Possible next steps:", StringSplit[#, "\n"] &, 
   LLMEvaluator -> llmParams];

evaluateProposal = 
  LLMFunction[
   "Evaluate if given numbers can reach 24 (sure/likely/impossible)
10 14
10 + 14 = 24
sure
11 12
11 + 12 = 23
12 - 11 = 1
11 * 12 = 132
11 / 12 = 0.91
impossible
4 4 10
4 + 4 + 10 = 8 + 10 = 18
4 * 10 - 4 = 40 - 4 = 36
(10 - 4) * 4 = 6 * 4 = 24
sure
4 9 11
9 + 11 + 4 = 20 + 4 = 24
sure
5 7 8
5 + 7 + 8 = 12 + 8 = 20
(8 - 5) * 7 = 3 * 7 = 21
I cannot obtain 24 now, but numbers are within a reasonable range
likely
5 6 6
5 + 6 + 6 = 17
(6 - 5) * 6 = 1 * 6 = 6
I cannot obtain 24 now, but numbers are within a reasonable range
likely
10 10 11
10 + 10 + 11 = 31
(11 - 10) * 10 = 10
10 10 10 are all too big
impossible
1 3 3
1 * 3 * 3 = 9
(1 + 3) * 3 = 12
1 3 3 are all too small
impossible
`input`", LLMEvaluator -> llmParams];

generateAnswer = 
  LLMFunction[
   "Use numbers and basic arithmetic operations (+ - * /) to obtain \
24. Each step, you are only allowed to choose two of the remaining \
numbers to obtain a new number.
Input: 4 4 6 8
Steps:
4 + 8 = 12 (left: 4 6 12)
6 - 4 = 2 (left: 2 12)
2 * 12 = 24 (left: 24)
Answer: (6 - 4) * (4 + 8) = 24
Input: 2 9 10 12
Steps:
12 * 2 = 24 (left: 9 10 24)
10 - 9 = 1 (left: 1 24)
24 * 1 = 24 (left: 24)
Answer: (12 * 2) * (10 - 9) = 24
Input: 4 9 10 13
Steps:
13 - 10 = 3 (left: 3 4 9)
9 - 3 = 6 (left: 4 6)
4 * 6 = 24 (left: 24)
Answer: 4 * (9 - (13 - 10)) = 24
Input: 1 4 8 8
Steps:
8 / 4 = 2 (left: 1 2 8)
1 + 2 = 3 (left: 3 8)
3 * 8 = 24 (left: 24)
Answer: (1 + 8 / 4) * 8 = 24
Input: 5 5 5 9
Steps:
5 + 5 = 10 (left: 5 9 10)
10 + 5 = 15 (left: 9 15)
15 + 9 = 24 (left: 24)
Answer: ((5 + 5) + 5) + 9 = 24
Input: `input`
Steps:
`steps`
", LLMEvaluator -> llmParams];

evaluateAnswer = 
  LLMFunction[
   "Use numbers and basic arithmetic operations (+ - * /) to obtain \
24. Given an input and an answer, give a judgement (sure/impossible) \
if the answer is correct, i.e. it uses each input exactly once and no \
other numbers, and reach 24.
Input: 4 4 6 8
Answer: (4 + 8) * (6 - 4) = 24
Judge: 
sure
Input: 2 9 10 12
Answer: 2 * 12 * (10 - 9) = 24
Judge: 
sure
Input: 4 9 10 13
Answer: (13 - 9) * (10 - 4) = 24
Judge: 
sure
Input: 4 4 6 8
Answer: (4 + 8) * (6 - 4) + 1 = 25
Judge: 
impossible
Input: 2 9 10 12
Answer: 2 * (12 - 10) = 24
Judge: 
impossible
Input: 4 9 10 13
Answer: (13 - 4) * (10 - 9) = 24
Judge: 
impossible
Input: `input`
Answer: `answer`
Judge:", LLMEvaluator -> llmParams];

(*Code to reproduce result graph*)
(*Tree["input: 4 5 6 \
10",Prepend[step1Thoughts[[2;;]],Tree[step1Thoughts[[1]],{1}]],\
TreeLayout->Left]*)
firstSteps = StringSplit[#, "\n"][[1]] & /@ results;
secondSteps = StringSplit[#, "\n"][[2]] & /@ results;
secondSteps = 
  MapThread[(StringJoin[#1, "\n", #2]) &, {firstSteps, secondSteps}];
thirdSteps = StringSplit[#, "\n"][[3]] & /@ results;
thirdSteps = 
  MapThread[(StringJoin[#1, "\n", #2]) &, {secondSteps, thirdSteps}];
fourthSteps = StringSplit[#, "\n"][[4]] & /@ results;
fourthSteps = 
  MapThread[(StringJoin[#1, "\n", #2]) &, {thirdSteps, fourthSteps}];

answer = StringTake[StringSplit[#, "\n"][[4]] &@results[[2]], {9, -1}];
answerIndex = 
  First[Select[Range[Length[results]], 
    Length[StringCases[results[[#]], 
        "Answer: " ~~ x__ ~~ "=" :> x]] > 0 & ]];
proposals2 = getProposals[firstSteps[[answerIndex]], taskInput];
values2 = getValues[proposals2, taskInput];

getBestProposals[proposals_, values_] := 
  Module[{ids, sortedIDs, selectIDs, nSelect = 5},
   ids = Range[Length[proposals]];
   sortedIDs = Reverse@SortBy[ids, values[[#]] &];
   selectIDs = Take[sortedIDs, Min[nSelect, Length[values]]];
   proposals[[#]] & /@ selectIDs
   ]; 

bestProposals2 = getBestProposals[proposals2, values2];
proposals3 = getProposals[secondSteps[[answerIndex]], taskInput];
values3 = getValues[proposals3, taskInput];
bestProposals3 = getBestProposals[proposals3, values3];

bestSteps2 = 
  StringReplace[StringSplit[#, "\n"][[2]], "(" -> "\n("] & /@ 
   bestProposals2;
treeData1 = StringReplace[#, "(" -> "\n("] & /@ firstSteps;
swapIndices[list_, i_, j_] := 
 Module[{temp = list}, temp[[{i, j}]] = temp[[{j, i}]];
  temp]
bestSteps2 = swapIndices[bestSteps2, 1, 3];
bestSteps3 = 
  StringReplace[StringSplit[#, "\n"][[3]], "(" -> "\n("] & /@ 
   bestProposals3;
bestSteps3 = swapIndices[bestSteps3, 1, 3];
bestSteps3[[3]] = 
  Tree[bestSteps3[[3]], {answer}, 
   TreeElementStyle -> {{1} -> LightGreen}];
bestSteps2[[3]] = Tree[bestSteps2[[3]], bestSteps3];
treeData1[[2]] = Tree[treeData1[[2]], bestSteps2];

treeStyle = { 
   {1} -> LightYellow, {3} -> LightYellow, {4} -> LightYellow, {5} -> 
    LightYellow,
   {_, 1} -> LightYellow, {_, 2} -> LightYellow , {_, 4} -> 
    LightYellow, {_, 5} -> LightYellow,
   {__, 1} -> LightYellow, {__, 2} -> LightYellow , {__, 4} -> 
    LightYellow
   };

(* Alternate implementation with ServiceConnect *)
openai = ServiceConnect["OpenAI", "New"];
openaiModel = "gpt-4";
(*chatCompletion[prompt_String,n_Integer,input_String] := *)
chatCompletion[params_Association] :=
  Module[{response, prompt, input, n, steps, contentParams, output},
   prompt = params["prompt"];
   input = params["input"];
   If[IntegerQ[params["n"]], n = params["n"], n = 1];
   If[KeyExistsQ[params, "steps"], steps = params["steps"], 
    steps = False];
   If[StringQ[steps], 
    contentParams = <|"input" -> input, "steps" -> steps|>, 
    contentParams = <|"input" -> input|>];
   response = 
    ServiceExecute[openai, 
     "Chat", {"Model" -> openaiModel, "N" -> n, "MaxTokens" -> 1000, 
      "Messages" -> {
        <|"role" -> "user", 
         "content" -> StringTemplate[prompt][contentParams]|>
        }}];
   (*Print[StringForm["response is ``.",response]];*)
   If[prompt == proposalPrompt,
    output = 
     StringSplit[#, "\n"] &@response[[1]]["message"]["content"],
    If[n == 1, 
     output = response[[1]]["message"]["content"],
     output = (#["message"]["content"]) & /@  response
     ]
    ];
   output
   ];
generateProposals[params_Association] := 
  chatCompletion[<|"prompt" -> proposalPrompt, 
    "input" -> params["input"]|>];
generateAnswer[params_Association] := 
  chatCompletion[<|"prompt" -> answerPrompt, 
    "input" -> params["input"], "steps" -> params["steps"]|>];
evaluateProposal[params_Association] := 
  chatCompletion[<|"prompt" -> evaluateProposalPrompt, 
    "input" -> params["input"], "n" -> params["n"]|>];
evaluateAnswer[params_Association] := 
  chatCompletion[<|"prompt" -> evaluateAnswerPrompt, 
    "input" -> params["input"], "answer" -> params["answer"], 
    "n" -> params["n"]|>];

cot1Shot = 
  LLMFunction[
   "Use numbers and basic arithmetic operations (+ - * /) to obtain 24.
Each step, you are only allowed to choose two of the remaining \
numbers to obtain a new number. You must use the four numbers in the \
input and only those numbers in your answer.
Input: 4 4 6 8
Steps:
4 + 8 = 12 (left: 4 6 12)
6 - 4 = 2 (left: 2 12)
2 * 12 = 24 (left: 24)
Answer: (6 - 4) * (4 + 8) = 24
Input: `input`", LLMEvaluator -> llmParams];

cot1ShotResponse = cot1Shot[<|"input" -> "4 9 10 13"|>];

cot5Shot = 
  LLMFunction[
   "Use numbers and basic arithmetic operations (+ - * /) to obtain \
24. Each step, you are only allowed to choose two of the remaining \
numbers to obtain a new number. You must use the four numbers in the \
input and only those numbers in your answer.
Input: 4 4 6 8
Steps:
4 + 8 = 12 (left: 4 6 12)
6 - 4 = 2 (left: 2 12)
2 * 12 = 24 (left: 24)
Answer: (6 - 4) * (4 + 8) = 24
Input: 2 9 10 12
Steps:
12 * 2 = 24 (left: 9 10 24)
10 - 9 = 1 (left: 1 24)
24 * 1 = 24 (left: 24)
Answer: (12 * 2) * (10 - 9) = 24
Input: 4 9 10 13
Steps:
13 - 10 = 3 (left: 3 4 9)
9 - 3 = 6 (left: 4 6)
4 * 6 = 24 (left: 24)
Answer: 4 * (9 - (13 - 10)) = 24
Input: 1 4 8 8
Steps:
8 / 4 = 2 (left: 1 2 8)
1 + 2 = 3 (left: 3 8)
3 * 8 = 24 (left: 24)
Answer: (1 + 8 / 4) * 8 = 24
Input: 5 5 5 9
Steps:
5 + 5 = 10 (left: 5 9 10)
10 + 5 = 15 (left: 9 15)
15 + 9 = 24 (left: 24)
Answer: ((5 + 5) + 5) + 9 = 24
Input: `input`
", LLMEvaluator -> llmParams];

cot5ShotResponse = cot5Shot[<|"input" -> "4 9 10 13"|>]

cot5ShotResponse = cot5Shot[<|"input" -> "3 4 5 13"|>]

generateProposals = LLMFunction["Input: 2 8 8 14
Possible next steps:
2 + 8 = 10 (left: 8 10 14)
8 / 2 = 4 (left: 4 8 14)
14 + 2 = 16 (left: 8 8 16)
2 * 8 = 16 (left: 8 14 16)
8 - 2 = 6 (left: 6 8 14)
14 - 8 = 6 (left: 2 6 8)
14 /  2 = 7 (left: 7 8 8)
14 - 2 = 12 (left: 8 8 12)
Input: `input`
Possible next steps:", StringSplit[#, "\n"] &, 
   LLMEvaluator -> llmParams];

taskInput = "4 5 6 10";
proposals = generateProposals[<|"input" -> taskInput|>]

Tree[StringTemplate["input: `input`"][<|"input" -> taskInput|>], 
 StringReplace[#, "(" -> "\n("] & /@ proposals, TreeLayout -> Left]

 evaluateProposal = 
  LLMFunction[
   "Evaluate if given numbers can reach 24 (sure/likely/impossible)
10 14
10 + 14 = 24
sure
11 12
11 + 12 = 23
12 - 11 = 1
11 * 12 = 132
11 / 12 = 0.91
impossible
4 4 10
4 + 4 + 10 = 8 + 10 = 18
4 * 10 - 4 = 40 - 4 = 36
(10 - 4) * 4 = 6 * 4 = 24
sure
4 9 11
9 + 11 + 4 = 20 + 4 = 24
sure
5 7 8
5 + 7 + 8 = 12 + 8 = 20
(8 - 5) * 7 = 3 * 7 = 21
I cannot obtain 24 now, but numbers are within a reasonable range
likely
5 6 6
5 + 6 + 6 = 17
(6 - 5) * 6 = 1 * 6 = 6
I cannot obtain 24 now, but numbers are within a reasonable range
likely
10 10 11
10 + 10 + 11 = 31
(11 - 10) * 10 = 10
10 10 10 are all too big
impossible
1 3 3
1 * 3 * 3 = 9
(1 + 3) * 3 = 12
1 3 3 are all too small
impossible
`input`", LLMEvaluator -> llmParams];

getLeftNumbers[thought_String] := 
  First@StringCases[thought, "(left: " ~~ x__ ~~ ")" :> x];

proposal = proposals[[1]];
evaluatedT = evaluateProposal[<|"input" -> getLeftNumbers[proposal]|>]

getValueHelper[evaluatedT_] := If[StringQ[evaluatedT],
   Switch[ToLowerCase[Last[StringSplit[evaluatedT, "\n"]]], "likely", 
    1, "sure", 20, "impossible", 0.1, _, 0],
   0
   ];

value = getValueHelper[evaluatedT]
proposals
values = getValues[proposals, taskInput]

valuedProposals = (Tree[
      StringReplace[proposals[[#]], 
       "(" -> "\n("], {StringForm["value = ``", 
        NumberForm[values[[#]], 2]  ]}]) & /@ Range[Length[proposals]];
Tree[StringTemplate["input: `input`"][<|
   "input" -> taskInput|>], valuedProposals, TreeLayout -> Left, 
 TreeElementStyle -> {{ _} -> LightYellow, {_, __} -> LightBlue}, 
 ImageSize -> 300]

valuedProposals2 = (Tree[
      StringReplace[proposals[[#]], 
       "(" -> "\n("], {StringForm["value = ``", 
        NumberForm[values[[#]], 2]  ]}]) & /@ 
   Range[Length[proposals]/2];
Tree[StringTemplate["input: `input`"][<|
   "input" -> taskInput|>], valuedProposals2, TreeLayout -> Left, 
 TreeElementStyle -> {{ _} -> LightYellow, {_, __} -> LightBlue}, 
 ImageSize -> 300]

nSelect = 5;
ids = Range[Length[proposals]];
sortedIDs = Reverse@SortBy[ids, values[[#]] &];
selectIDs = Take[sortedIDs, Min[nSelect, Length[sortedIDs]]];
newProposals = proposals[[#]] & /@ selectIDs

valuedProposals = (Tree[
      proposals[[#]], {StringForm["value = ``", 
        NumberForm[values[[#]], 2]  ]}]) & /@ 
   Range[Length[proposals]];

Tree[StringTemplate["input: `input`"][<|"input" -> taskInput|>], 
 StringReplace[#, "(" -> "\n("] & /@ newProposals, TreeLayout -> Left,
  TreeElementStyle -> {{ _} -> LightYellow}, ImageSize -> 200]

proposals = Flatten[getProposals[#, taskInput] & /@ newProposals];
first = StringSplit[proposals[[1]], "\n"][[1]];
split = StringSplit[#, "\n"] & /@ proposals;
filtered = (#[[2]] &) /@ Select[split, #[[1]] == first &];

Tree[first, StringReplace[#, "(" -> "\n("] & /@ filtered, 
 TreeLayout -> Left, 
 TreeElementStyle -> {{ } -> LightYellow, {_} -> LightOrange}, 
 ImageSize -> 250]

generateAnswer = 
  LLMFunction[
   "Use numbers and basic arithmetic operations (+ - * /) to obtain \
24. Each step, you are only allowed to choose two of the remaining \
numbers to obtain a new number.
Input: 4 4 6 8
Steps:
4 + 8 = 12 (left: 4 6 12)
6 - 4 = 2 (left: 2 12)
2 * 12 = 24 (left: 24)
Answer: (6 - 4) * (4 + 8) = 24
Input: 2 9 10 12
Steps:
12 * 2 = 24 (left: 9 10 24)
10 - 9 = 1 (left: 1 24)
24 * 1 = 24 (left: 24)
Answer: (12 * 2) * (10 - 9) = 24
Input: 4 9 10 13
Steps:
13 - 10 = 3 (left: 3 4 9)
9 - 3 = 6 (left: 4 6)
4 * 6 = 24 (left: 24)
Answer: 4 * (9 - (13 - 10)) = 24
Input: 1 4 8 8
Steps:
8 / 4 = 2 (left: 1 2 8)
1 + 2 = 3 (left: 3 8)
3 * 8 = 24 (left: 24)
Answer: (1 + 8 / 4) * 8 = 24
Input: 5 5 5 9
Steps:
5 + 5 = 10 (left: 5 9 10)
10 + 5 = 15 (left: 9 15)
15 + 9 = 24 (left: 24)
Answer: ((5 + 5) + 5) + 9 = 24
Input: `input`
Steps:
`steps`
", LLMEvaluator -> llmParams];

taskInput = "4 5 6 10";
steps = "
5 * 4 = 20 (left: 6 10 20)
10 + 20 = 30 (left: 6 30)
30 - 6 = 24 (left: 24)";
answer = generateAnswer[<|"input" -> taskInput, "steps" -> steps|>]

evaluateAnswer = 
  LLMFunction[
   "Use numbers and basic arithmetic operations (+ - * /) to obtain \
24. Given an input and an answer, give a judgement (sure/impossible) \
if the answer is correct, i.e. it uses each input exactly once and no \
other numbers, and reach 24.
Input: 4 4 6 8
Answer: (4 + 8) * (6 - 4) = 24
Judge: 
sure
Input: 2 9 10 12
Answer: 2 * 12 * (10 - 9) = 24
Judge: 
sure
Input: 4 9 10 13
Answer: (13 - 9) * (10 - 4) = 24
Judge: 
sure
Input: 4 4 6 8
Answer: (4 + 8) * (6 - 4) + 1 = 25
Judge: 
impossible
Input: 2 9 10 12
Answer: 2 * (12 - 10) = 24
Judge: 
impossible
Input: 4 9 10 13
Answer: (13 - 4) * (10 - 9) = 24
Judge: 
impossible
Input: `input`
Answer: `answer`
Judge:", LLMEvaluator -> llmParams];

evaluatedAnswer = 
 evaluateAnswer[<|"input" -> taskInput, "answer" -> answer|>]

(*getProposals [steps_String,taskInput_String] :=*)
solveTask[taskInput_] :=
  Module[{nSteps = 4, nSelect = 5, proposals, newProposals, values, 
    ids, sortedIDs, selectIDs},
   proposals = {""};
   Do[
    newProposals = 
     Flatten[getProposals[#, taskInput] & /@ proposals];
    values = getValues[newProposals, taskInput];
    ids = Range[Length[newProposals]];
    sortedIDs = Reverse@SortBy[ids, values[[#]] &];
    selectIDs = Take[sortedIDs, Min[nSelect, Length[sortedIDs]]];
    proposals = newProposals[[#]] & /@ selectIDs,
    {i, nSteps}
    ];
   proposals
   ];

results = solveTask["4 5 6 10"]

Tree[StringTemplate["input: `input`"][<|
   "input" -> taskInput|>], treeData1, TreeLayout -> Left, 
 TreeElementStyle -> treeStyle, ImageSize -> 600]


(* coding challenges *)
exercise1 = 
  "Make a combined list of the first 5 squares and cubes (numbers \
raised to the power 3), sorted into order.";
exercise2 = "Find the second-to-last digit of 2^32.";
exercise3 = 
  "Make a graph with 50 nodes, in which node i connects to node \
i+1.";
exercise4 = 
  "Make a line plot of the numerical phase of the moon for each of \
the next 30 days.";
exercise5 = 
  "In the digit lists for the first 1000 squares, find those that \
begin with 9 and end with 0 or 1.";

getSteps = 
  evaluateProposal = 
   LLMFunction["Here is a programming task in Wolfram language:
`input`
How would you break this problem down into a step by step procedure \
you could use to arrive at an answer?", LLMEvaluator -> llmParams];

steps1 = getSteps[<|"input" -> exercise1|>]

proposalPrompt = 
  "Given a task and a list of steps generate the likely next step \
that gets closer to achieving the task.
Task: Make a combined list of the first 5 squares and cubes (numbers \
raised to the power 3), sorted into order.
Steps:
1. Generate a list of the first five natural numbers. In Wolfram \
Language, you can do this with the Range[] function, as Range[5].
2. Square each number in the list. This can be done by applying the \
power operator ^ to the list, as Range[5]^2. Wolfram Language will \
automatically apply the operation to each element of the list.
3. Cube each number in the list. This is done in the same way as the \
squaring operation, but with 3 as the exponent, as Range[5]^3.
Possible next step:
4. Join the list of squares and the list of cubes into a single list. \
The Join[] function can be used for this, as Join[Range[5]^2, \
Range[5]^3].
Task: Make a line plot of the numerical phase of the moon for each of \
the next 30 days.
Steps:
1. Generate a list of the next 30 days. In Wolfram Language, you can \
use the Today function to get today's date, and you can add n days to \
this date with Today + n Quantity[1, \"Days\"]. If you use the \
Table[] function to do this for n from 0 to 29, you get a list of the \
dates of the next 30 days.
Possible next step:
2. For each date in this list, calculate the numerical phase of the \
moon. The MoonPhase[] function in Wolfram Language does exactly this. \
So Table[MoonPhase[Today + n Quantity[1, \"Days\"]], {n, 30}] \
generates a list of the moon phases for the next 30 days
Task: In the digit lists for the first 1000 squares, find those that \
begin with 9 and end with 0 or 1.
Steps:
1. Generate a list of the squares of the first 1000 natural numbers. \
This can be done using the Range[] function and the power operator ^, \
as Range[1000]^2.
2. Convert each number in this list to a list of its digits. The \
IntegerDigits[] function can be used for this, so \
IntegerDigits[Range[1000]^2] gives a list of lists, where each inner \
list is the digit list of a square.
Possible next step:
3. Search this list of lists for lists that begin with 9 and end with \
either 0 or 1. The Cases[] function in Wolfram Language can be used \
for this. The pattern {9, __, 0 | 1} matches any list that begins \
with 9 (9) and ends with either 0 or 1 (0 | 1), with any number of \
any digits in between (__). Therefore, \
Cases[IntegerDigits[Range[1000]^2], {9, __, 0 | 1}] returns all lists \
of digits that represent squares beginning with 9 and ending with 0 \
or 1.
Input: `input`
Steps: 
`steps`
Possible next step:
";

evaluateProposalPrompt = "Evaluate if following the procedure given \
in the steps will accomplish the given task (sure/likely/impossible)
Task: Find the second-to-last digit of 2^32.
Steps:
1. Calculate the number 2^32. In Wolfram Language, this is done \
simply as 2^32.
2. Convert the result into a list of digits. Wolfram Language \
provides the function IntegerDigits[] for this purpose.
3. Reverse the order of the list of digits. This can be done with the \
Reverse[] function. Reversing the list of digits will allow us to \
directly address the second-to-last digit as the second element of \
the reversed list (since Wolfram Language uses 1-based indexing).
4. Extract the second element from the reversed list of digits. This \
can be done with the Part[] function (which is often abbreviated as \
[[ ]]), using 2 as the index.
sure
Task: Make a combined list of the first 5 squares and cubes (numbers \
raised to the power 3), sorted into order.
Steps:
1. Generate a list of the first five natural numbers. In Wolfram \
Language, you can do this with the Range[] function, as Range[5].
2. Square each number in the list. This can be done by applying the \
power operator ^ to the list, as Range[5]^2. Wolfram Language will \
automatically apply the operation to each element of the list.
3. Cube each number in the list. This is done in the same way as the \
squaring operation, but with 3 as the exponent, as Range[5]^3.
I cannot accomplish the task now, but the steps are reasonable
likely
Task: Make a graph with 50 nodes, in which node i connects to node i+1.
Steps:
1. Generate a list of numbers from 1 to 49. In Wolfram Language, you \
can do this using the Table[] function as Table[i, {i, 49}]. However, \
for this task, we are not just generating a list of numbers; we are \
generating a list of directed edges between nodes.
2. For each number i in this list, create an edge from node i + 1 to \
node i. This can be done using the -> operator in Wolfram Language. \
So the command Table[i + 1 -> i, {i, 49}] generates a list of edges \
where each node i (for i from 1 to 49) is connected to node i + 1.
3. Create a graph from this list of edges. Wolfram Language has a \
Graph[] function that can take a list of edges as input and generate \
a corresponding graph.
impossible
Task: Make a line plot of the numerical phase of the moon for each of \
the next 30 days.
Steps: 
1. Generate a list of the next 30 days. In Wolfram Language, you can \
use the Today function to get today's date, and you can add n days to \
this date with Today + n Quantity[1, \"Days\"]. If you use the \
Table[] function to do this for n from 0 to 29, you get a list of the \
dates of the next 30 days.
2. For each date in this list, calculate the numerical phase of the \
moon. The MoonPhase[] function in Wolfram Language does exactly this. \
So Table[MoonPhase[Today + n + 1 Quantity[1, \"Days\"]], {n, 30}] \
generates a list of the moon phases for the next 30 days
3. Create a line plot of this list of moon phases. You can use the \
ListLinePlot[] function for this. Therefore, \
ListLinePlot[Table[MoonPhase[Today + n + 1 Quantity[1, \"Days\"]], \
{n, 30}]] creates the desired plot.
impossible
Task: In the digit lists for the first 1000 squares, find those that \
begin with 9 and end with 0 or 1.
Steps:
1. Generate a list of the squares of the first 1000 natural numbers. \
This can be done using the Range[] function and the power operator ^, \
as Range[1000]^2.
2. Convert each number in this list to a list of its digits. The \
IntegerDigits[] function can be used for this, so \
IntegerDigits[Range[1000]^2] gives a list of lists, where each inner \
list is the digit list of a square.
3. Search this list of lists for lists that begin with 9 and end with \
either 0 or 1. The Cases[] function in Wolfram Language can be used \
for this. The pattern {9, __, 0 | 1} matches any list that begins \
with 9 (9) and ends with either 0 or 1 (0 | 1), with any number of \
any digits in between (__). Therefore, \
Cases[IntegerDigits[Range[1000]^2], {9, __, 0 | 1}] returns all lists \
of digits that represent squares beginning with 9 and ending with 0 \
or 1.
sure
Task: `input`
Steps:
`steps`
"

answerPrompt = 
  "Follow the steps given to write a single line of Wolfram language \
code that satisfies the given task.
Task: Find the second-to-last digit of 2^32.
Steps:
1. Calculate the number 2^32. In Wolfram Language, this is done \
simply as 2^32.
2. Convert the result into a list of digits. Wolfram Language \
provides the function IntegerDigits[] for this purpose.
3. Reverse the order of the list of digits. This can be done with the \
Reverse[] function. Reversing the list of digits will allow us to \
directly address the second-to-last digit as the second element of \
the reversed list (since Wolfram Language uses 1-based indexing).
4. Extract the second element from the reversed list of digits. This \
can be done with the Part[] function (which is often abbreviated as \
[[ ]]), using 2 as the index.
Answer: (2^32 // IntegerDigits // Reverse)[[2]]
Task: Make a combined list of the first 5 squares and cubes (numbers \
raised to the power 3), sorted into order.
Steps:
1. Generate a list of the first five natural numbers. In Wolfram \
Language, you can do this with the Range[] function, as Range[5].
2. Square each number in the list. This can be done by applying the \
power operator ^ to the list, as Range[5]^2. Wolfram Language will \
automatically apply the operation to each element of the list.
3. Cube each number in the list. This is done in the same way as the \
squaring operation, but with 3 as the exponent, as Range[5]^3.
4. Join the list of squares and the list of cubes into a single list. \
The Join[] function can be used for this, as Join[Range[5]^2, \
Range[5]^3].
5. Sort the combined list into order. This can be done with the \
Sort[] function, as Sort[Join[Range[5]^2, Range[5]^3]].
Answer: Sort[Join[Range[5]^2,Range[5]^3]]
Task: Make a graph with 50 nodes, in which node i connects to node i+1.
Steps:
1. Generate a list of numbers from 1 to 49. In Wolfram Language, you \
can do this using the Table[] function as Table[i, {i, 49}]. However, \
for this task, we are not just generating a list of numbers; we are \
generating a list of directed edges between nodes.
2. For each number i in this list, create an edge from node i to node \
i + 1. This can be done using the -> operator in Wolfram Language. So \
the command Table[i -> i + 1, {i, 49}] generates a list of edges \
where each node i (for i from 1 to 49) is connected to node i + 1.
3. Create a graph from this list of edges. Wolfram Language has a \
Graph[] function that can take a list of edges as input and generate \
a corresponding graph.
Answer: Graph[Table[i -> i + 1, {i, 49}]]
Task: Make a line plot of the numerical phase of the moon for each of \
the next 30 days.
Steps:
1. Generate a list of the next 30 days. In Wolfram Language, you can \
use the Today function to get today's date, and you can add n days to \
this date with Today + n Quantity[1, \"Days\"]. If you use the \
Table[] function to do this for n from 0 to 29, you get a list of the \
dates of the next 30 days.
2. For each date in this list, calculate the numerical phase of the \
moon. The MoonPhase[] function in Wolfram Language does exactly this. \
So Table[MoonPhase[Today + n Quantity[1, \"Days\"]], {n, 30}] \
generates a list of the moon phases for the next 30 days
3. Create a line plot of this list of moon phases. You can use the \
ListLinePlot[] function for this. Therefore, \
ListLinePlot[Table[MoonPhase[Today + n Quantity[1, \"Days\"]], {n, \
30}]] creates the desired plot.
Answer: ListLinePlot[Table[MoonPhase[Today + n \
Quantity[1,\"Days\"]],{n,30}]]
Task: In the digit lists for the first 1000 squares, find those that \
begin with 9 and end with 0 or 1.
Steps:
1. Generate a list of the squares of the first 1000 natural numbers. \
This can be done using the Range[] function and the power operator ^, \
as Range[1000]^2.
2. Convert each number in this list to a list of its digits. The \
IntegerDigits[] function can be used for this, so \
IntegerDigits[Range[1000]^2] gives a list of lists, where each inner \
list is the digit list of a square.
3. Search this list of lists for lists that begin with 9 and end with \
either 0 or 1. The Cases[] function in Wolfram Language can be used \
for this. The pattern {9, __, 0 | 1} matches any list that begins \
with 9 (9) and ends with either 0 or 1 (0 | 1), with any number of \
any digits in between (__). Therefore, \
Cases[IntegerDigits[Range[1000]^2], {9, __, 0 | 1}] returns all lists \
of digits that represent squares beginning with 9 and ending with 0 \
or 1.
Answer: Cases[IntegerDigits[Range[1000]^2],{9,__,0 | 1}]
Task: `input`
Steps:
`steps`
";

evaluateAnswerPrompt = "Given a task and an answer, give a judgement \
(sure/impossible) if the answer is correct, i.e. the line of code \
provided in the answer, when evaluated will give the result specified \
in the task.
Task: Find the second-to-last digit of 2^32.
Answer: (2^32 // IntegerDigits // Reverse)[[2]]
Judge: 
sure
Task: Find the second-to-last digit of 2^32.
Answer: (2^32 // IntegerDigits // Reverse)[[1]]
Judge: 
impossible
Task: Make a graph with 50 nodes, in which node i connects to node i+1.
Answer: Graph[Table[i -> i + 1, {i, 49}]]
Judge:
sure
Task: Make a graph with 50 nodes, in which node i connects to node i+1.
Answer: Graph[Table[i -> i + 1, {i, 50}]]
Judge:
impossible
Task: Make a combined list of the first 5 squares and cubes (numbers \
raised to the power 3), sorted into order.
Answer: Sort[Join[Range[5]^2,Range[5]^3]]
Judge:
sure
Task: Make a combined list of the first 5 squares and cubes (numbers \
raised to the power 3), sorted into order.
Answer: Sort[Table[i^2, {i, 5}], Table[i^3, {i, 5}]]
Judge:
impossible
Task: `input`
Answer: `answer`
Judge:
"