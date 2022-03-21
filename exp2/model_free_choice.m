% model free calculation
merged_choice = xlsread('merged_choice.xlsx');
data = merged_choice;
subjects = unique(data(:,1));  


generosity = [];
general_inequity = [];
inequity_a = [];
inequity_b = [];

for nsub = 1:length(subjects)
    sub = subjects(nsub);
    subdata = data(data(:,1) == sub,:); % 提取单个被试的数据
    gene_sub = [];
    gi_sub = [];
    gi_a_sub = [];
    gi_b_sub = [];
    
    for k =1:4
        other = 0; % binary choice
        gi = 0;
        gi_a = 0;
        gi_b = 0;
        count_a = 0;
        count_b = 0;
        for j=1:length(subdata)
            if subdata(j,k*3)>subdata(j,k*3+1) % 是优势不公平情况
                count_a = count_a+1;
            elseif subdata(j,k*3)<subdata(j,k*3+1) % 是劣势不公平情况
                count_b = count_b+1;
            end
            
            if subdata(j,k*3+2)==1 % 选择不公平选项
                other = other + subdata(j,k*3+1);
                if subdata(j,k*3)>subdata(j,k*3+1)
                    gi = gi+subdata(j,k*3)-subdata(j,k*3+1);
                    gi_a = gi_a+subdata(j,k*3)-subdata(j,k*3+1);
                elseif subdata(j,k*3)<subdata(j,k*3+1)
                    gi = gi+subdata(j,k*3+1)-subdata(j,k*3);
                    gi_b = gi_b+subdata(j,k*3+1)-subdata(j,k*3);
                end 
            elseif subdata(j,k*3+2)==2 % 选择公平选项
                other = other + 10;
            end
        end
        
        other = other/length(subdata);
        gi = gi/length(subdata);
        gi_a = gi_a/count_a;
        gi_b = gi_b/count_b;
        
        gene_sub = [gene_sub, other];
        gi_sub = [gi_sub, gi];
        gi_a_sub = [gi_a_sub,gi_a];
        gi_b_sub = [gi_b_sub,gi_b];
    end
    
    generosity = [generosity;gene_sub];
    generosity_a = [generosity_a;gene_a_sub];
    generosity_b = [generosity_b;gene_b_sub];
    selfishness = [selfishness;self_sub];
    general_inequity = [general_inequity;gi_sub];
    inequity_a = [inequity_a;gi_a_sub];
    inequity_b = [inequity_b;gi_b_sub];
end





