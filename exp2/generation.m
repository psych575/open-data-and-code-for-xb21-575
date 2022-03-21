% generate fake behavoir data

choice = xlsread('choice_sets.xlsx'); 

AIA = [0.5,0.6,0.2,0.3,0.4,-0.5,-0.6,-0.2,-0.3,-0.4];
DIA = [-0.5,-0.2,-0.6,-0.4,-0.3,0.5,0.2,0.6,0.4,0.3];
lambda = 0.5;
sub = 100; % ������100����ٱ��Ե�����
t = 48; % ��48���Դ�

chosen1 = [choice,zeros(t,1)];
chosen2 = [choice,zeros(t,1)];

for i = 1:sub
    alpha = AIA(ceil(i/10));
    beta = DIA(ceil(i/10));
    for j = 1:t
        Ms = choice(j,1);
        Mo = choice(j,2);
        U = Ms - alpha*(Ms>=Mo)*(Ms-Mo) - beta*(Mo>Ms)*(Mo-Ms);
        U_standard = 10;
        U_delta = U - U_standard;
        p = 1/(1+exp(-lambda*U_delta)); % ѡ�񲻹�ƽ�Դεĸ���
        result = binornd(1,p);
        if i <= 50
            chosen1(j,3) = chosen1(j,3) + result;
        else
            chosen2(j,3) = chosen2(j,3) + result;
        end
    end
end 

chosen1(:,3) = chosen1(:,3)/50;
chosen2(:,3) = chosen2(:,3)/50;
        
        


