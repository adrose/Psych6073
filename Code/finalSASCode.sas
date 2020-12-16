/* Repeated measures problem, Dataset 1 */
/* You have four therapies and four time measures, pre and three post measures.
Assume that the measures are comparable.
Run the analyses using proc glm and proc mixed*/
data Set1;
input subj group therapy $ pre post1-post3; cards;
1	1	Control	48	68	72	74
2	1	Control	52	46	55	61
3	1	Control	55	50	56	51
4	1	Control	50	43	48	45
5	1	Control	49	76	54	60
6	1	Control	44	59	46	62
7	1	Control	52	40	52	35
8	1	Control	41	36	43	38
9	1	Control	60	40	58	46
10	1	Control	61	53	56	49
11	2	Cognitive	52	48	66	65
12	2	Cognitive	53	61	68	66
13	2	Cognitive	56	56	60	70
14	2	Cognitive	47	53	40	54
15	2	Cognitive	34	52	53	67
16	2	Cognitive	59	45	73	68
17	2	Cognitive	42	47	43	66
18	2	Cognitive	53	59	72	71
19	2	Cognitive	53	47	49	44
20	2	Cognitive	50	39	76	59
21	3	Behavioral	57	72	61	90
22	3	Behavioral	57	57	73	74
23	3	Behavioral	55	48	54	57
24	3	Behavioral	47	43	57	59
25	3	Behavioral	42	34	57	54
26	3	Behavioral	55	47	73	69
27	3	Behavioral	42	55	52	58
28	3	Behavioral	56	54	74	67
29	3	Behavioral	30	48	48	68
30	3	Behavioral	49	44	62	66
31	4	Abreaction	63	63	66	67
32	4	Abreaction	48	37	56	70
33	4	Abreaction	52	53	67	91
34	4	Abreaction	65	55	58	58
35	4	Abreaction	43	62	52	75
36	4	Abreaction	41	40	57	73
37	4	Abreaction	55	37	65	58
38	4	Abreaction	48	44	46	53
39	4	Abreaction	53	57	67	74
40	4	Abreaction	46	41	58	61
;

PROC GLM DATA=Set1;
CLASS subj group;
MODEL pre post1-post3 = group ;
repeated  time 4 / printe;
run;


proc import datafile = '/home/arosen/Documents/Psych6073/Data/set1Long.csv' out=Set1 dbms=csv
replace;
run;

proc mixed data=Set1; 
class variable therapy subj ; 
model value = variable therapy variable*therapy ;
random subj;
run;


