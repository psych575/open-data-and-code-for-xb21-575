% model free calculation for choice
merged_choice = xlsread('merged_choice.xlsx');
data = merged_choice;
subjects = unique(data(:,7));  


generosity = [];  % 平均分给对方的点数
general_inequity = []; % 总体的自我他人收益之差（不区分优势和劣势）
gi_a = []; % 优势条件下自我他人收益之差
gi_b = []; % 劣势条件下自我他人收益之差

for nsub = 1:length(subjects)
    sub = subjects(nsub);
    subdata = data(data(:,7) == sub,:); % 提取单个被试的数据
    other1 = 0; % 可见条件下分给他人的点数
    other2 = 0; % 不可见条件下分给他人的点数
    gi1 = 0; 
    gi2 = 0; 
    gi_a1 = 0;
    gi_a2 = 0;
    gi_b1 = 0;
    gi_b2 = 0;
    count_a1 = 0; % 储存优势和劣势试次的数量
    count_a2 = 0;
    count_b1 = 0;
    count_b2 = 0;
    
    for j=1:length(subdata)
        if subdata(j,1)>subdata(j,2) % 是优势不公平情况
            count_a1 = count_a1+1;
        elseif subdata(j,1)<subdata(j,2) % 是劣势不公平情况
            count_b1 = count_b1+1;
        end
        
        if subdata(j,3)==1 % 选择不公平选项
            other1 = other1 + subdata(j,2);
            if subdata(j,1)>subdata(j,2)
                gi1 = gi1+subdata(j,1)-subdata(j,2);
                gi_a1 = gi_a1+subdata(j,1)-subdata(j,2);
            elseif subdata(j,1)<subdata(j,2)
                gi1 = gi1+subdata(j,2)-subdata(j,1);
                gi_b1 = gi_b1+subdata(j,2)-subdata(j,1);
            end
            
        elseif subdata(j,3)==2 % 选择公平选项
            other1 = other1 + 10;
        end
        
        if subdata(j,4)>subdata(j,5) % 是优势不公平情况
            count_a2 = count_a2+1;
        elseif subdata(j,4)<subdata(j,5) % 是劣势不公平情况
            count_b2 = count_b2+1;
        end
        
        if subdata(j,6)==1 % 选择不公平选项
            other2 = other2 + subdata(j,5);
            if subdata(j,4)>subdata(j,5)
                gi2 = gi2+subdata(j,4)-subdata(j,5);
                gi_a2 = gi_a2+subdata(j,4)-subdata(j,5);
            elseif subdata(j,4)<subdata(j,5)
                gi2 = gi2+subdata(j,5)-subdata(j,4);
                gi_b2 = gi_b2 + subdata(j,5)-subdata(j,4);
            end
        elseif subdata(j,6)==2 % 选择公平选项
            other2 = other2 + 10;
        end
    end
    
    % 计算单个被试的所有指标
    other1 = other1/length(subdata);
    other2 = other2/length(subdata);
    gi1 = gi1/(count_a1+count_b1);
    gi2 = gi2/(count_a2+count_b2);
    gi_a1 = gi_a1/count_a1;
    gi_a2 = gi_a2/count_a2;
    gi_b1 = gi_b1/count_b1;
    gi_b2 = gi_b2/count_b2;
    % 储存单个被试的所有指标
    generosity = [generosity;other1,other2];
    general_inequity = [general_inequity;gi1,gi2];
    gi_a = [gi_a;gi_a1,gi_a2];
    gi_b = [gi_b;gi_b1,gi_b2];
end





