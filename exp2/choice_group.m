
% group level estimates

global bootData

% load data
data = xlsread('merged_choice.xlsx');
outdir= 'C:\Users\Lenovo\Desktop\design\สตั้ถþ'; 

subjects = unique(data(:,1));  
pairData = data;

% bootstrap
nboot = 2000;
q0 = [0.4,0.5,0.6,0.5,0.5,0.5,0.5,0.5,0.5];
results = [];

[bootstat,bootsam] = bootstrp(nboot,@mean,pairData);

for n = 1801:nboot
    bootData = pairData(bootsam(:,n),:);
    n-1800
    
    [q, fval, exitflag] = fmincon(@choice_function, q0 ,[],[],[],[],[-5,-5,-5,-5,-5,-5,-5,-5,0],[5,5,5,5,5,5,5,5,5], [], optimset('Algorithm','interior-point'));
    results = [results; [n, fval, q, q0]];
end

% save the data
dlmwrite(fullfile(outdir, 'group_para.txt'),results,'delimiter','\t');







