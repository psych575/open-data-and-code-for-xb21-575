----介绍数据与程序的使用-实验二----

【选项集】
choice_set.xlsx是实验二中独裁者游戏使用的选项集

【生成虚拟被试的选择】
genetation.m用于生成虚拟被试的选择行为和选择比例，用以操纵社会规范，生成结果见choice_set.xlsx的第三列、第四列

【不依赖模型的分析】
(1)数据是merged_choice.xlsx，共14列，每一名被试占48行。其中每一列表示：
[被试编号，试次编号，利他-可见条件下的Ms，利他-可见条件下的Mo，利他-可见条件下的选择(1-选择不公平选项)，利他-不可见条件下的Ms，利他-不可见条件下的Mo，利他-不可见条件下的选择，不利他-可见条件下的Ms，不利他-可见条件下的Mo，不利他-可见条件下的选择，不利他-不可见条件下的Ms，不利他-不可见条件下的Mo，不利他-不可见条件下的选择]
(2)用程序model_free_choice.m计算不依赖模型的指标，整理为每个被试1行的数据

【基于模型的分析】
(1)数据是merged_choice_bayesian.csv
(2)用程序bayesian_model_exp2进行分层贝叶斯模型拟合

【加工好的数据】
()加工好个体水平数据（包括被试信息，不依赖模型的指标，基于模型的指标）保存在data_all.sav里，可直接用SPSS进行统计分析




