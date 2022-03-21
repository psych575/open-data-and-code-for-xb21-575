% model free calculation for ratings
merged_rating = xlsread('merged_rating.xlsx');
data = merged_rating;
subjects = unique(data(:,9));  

satis_a = []; % 储存优势条件下的评分
satis_b = []; % 储存劣势条件下的评分
satis = []; % 储存总的评分值

for nsub = 1:length(subjects)
    sub = subjects(nsub);
    subdata = data(data(:,9) == sub,:); % 提取单个被试的数据
    count_a1 = 0;
    count_a2 = 0;
    count_b1 = 0;
    count_b2 = 0;
    satis1 = 0;
    satis2 = 0;
    satis_a1 = 0;
    satis_a2 = 0;
    satis_b1 = 0;
    satis_b2 = 0;
    
    for j=1:length(subdata)
        if subdata(j,3)==1 % 对不公平选项的评分
            if subdata(j,1)>subdata(j,2) % 是优势不公平情况
                count_a1 = count_a1+1;
                satis_a1 = satis_a1 + subdata(j,4);
            elseif subdata(j,1)<subdata(j,2) % 是劣势不公平情况
                count_b1 = count_b1+1;
                satis_b1 = satis_b1 + subdata(j,4);
            end
            satis1 = satis1 + subdata(j,4);
        end
        
        if subdata(j,7)==1 
            if subdata(j,5)>subdata(j,6) % 是优势不公平情况
                count_a2 = count_a2+1;
                satis_a2 = satis_a2 + subdata(j,8);
            elseif subdata(j,5)<subdata(j,6) % 是劣势不公平情况
                count_b2 = count_b2+1;
                satis_b2 = satis_b2 + subdata(j,8);
            end
            satis2 = satis2 + subdata(j,8);
        end
    end
    
    satis1 = satis1/length(subdata);
    satis2 = satis2/length(subdata);
    satis_a1 = satis_a1/count_a1;
    satis_a2 = satis_a2/count_a2;
    satis_b1 = satis_b1/count_b1;
    satis_b2 = satis_b2/count_b2;
    satis_a = [satis_a;satis_a1,satis_a2];
    satis_b = [satis_b;satis_b1,satis_b2];
    satis = [satis;satis1,satis2];
end




