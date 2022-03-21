% model free calculation for choice
merged_choice = xlsread('merged_choice.xlsx');
data = merged_choice;
subjects = unique(data(:,7));  


generosity = [];  % ƽ���ָ��Է��ĵ���
general_inequity = []; % �����������������֮����������ƺ����ƣ�
gi_a = []; % ����������������������֮��
gi_b = []; % ����������������������֮��

for nsub = 1:length(subjects)
    sub = subjects(nsub);
    subdata = data(data(:,7) == sub,:); % ��ȡ�������Ե�����
    other1 = 0; % �ɼ������·ָ����˵ĵ���
    other2 = 0; % ���ɼ������·ָ����˵ĵ���
    gi1 = 0; 
    gi2 = 0; 
    gi_a1 = 0;
    gi_a2 = 0;
    gi_b1 = 0;
    gi_b2 = 0;
    count_a1 = 0; % �������ƺ������Դε�����
    count_a2 = 0;
    count_b1 = 0;
    count_b2 = 0;
    
    for j=1:length(subdata)
        if subdata(j,1)>subdata(j,2) % �����Ʋ���ƽ���
            count_a1 = count_a1+1;
        elseif subdata(j,1)<subdata(j,2) % �����Ʋ���ƽ���
            count_b1 = count_b1+1;
        end
        
        if subdata(j,3)==1 % ѡ�񲻹�ƽѡ��
            other1 = other1 + subdata(j,2);
            if subdata(j,1)>subdata(j,2)
                gi1 = gi1+subdata(j,1)-subdata(j,2);
                gi_a1 = gi_a1+subdata(j,1)-subdata(j,2);
            elseif subdata(j,1)<subdata(j,2)
                gi1 = gi1+subdata(j,2)-subdata(j,1);
                gi_b1 = gi_b1+subdata(j,2)-subdata(j,1);
            end
            
        elseif subdata(j,3)==2 % ѡ��ƽѡ��
            other1 = other1 + 10;
        end
        
        if subdata(j,4)>subdata(j,5) % �����Ʋ���ƽ���
            count_a2 = count_a2+1;
        elseif subdata(j,4)<subdata(j,5) % �����Ʋ���ƽ���
            count_b2 = count_b2+1;
        end
        
        if subdata(j,6)==1 % ѡ�񲻹�ƽѡ��
            other2 = other2 + subdata(j,5);
            if subdata(j,4)>subdata(j,5)
                gi2 = gi2+subdata(j,4)-subdata(j,5);
                gi_a2 = gi_a2+subdata(j,4)-subdata(j,5);
            elseif subdata(j,4)<subdata(j,5)
                gi2 = gi2+subdata(j,5)-subdata(j,4);
                gi_b2 = gi_b2 + subdata(j,5)-subdata(j,4);
            end
        elseif subdata(j,6)==2 % ѡ��ƽѡ��
            other2 = other2 + 10;
        end
    end
    
    % ���㵥�����Ե�����ָ��
    other1 = other1/length(subdata);
    other2 = other2/length(subdata);
    gi1 = gi1/(count_a1+count_b1);
    gi2 = gi2/(count_a2+count_b2);
    gi_a1 = gi_a1/count_a1;
    gi_a2 = gi_a2/count_a2;
    gi_b1 = gi_b1/count_b1;
    gi_b2 = gi_b2/count_b2;
    % ���浥�����Ե�����ָ��
    generosity = [generosity;other1,other2];
    general_inequity = [general_inequity;gi1,gi2];
    gi_a = [gi_a;gi_a1,gi_a2];
    gi_b = [gi_b;gi_b1,gi_b2];
end





