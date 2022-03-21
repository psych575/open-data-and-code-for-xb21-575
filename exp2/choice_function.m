
function y = choice_function(q)

global bootData

lambda_0 = q(9);

for t = 1:size(bootData,1)
    for x=1:4
        
        rho = q(x);
        sigma = q(x+4);
        lambda = lambda_0;

        Ms = bootData(t,x*3);     % Ms
        Mo = bootData(t,x*3+1);   % Mo
        
        U = Ms - rho*(Ms>=Mo)*(Ms-Mo) - sigma*(Mo>Ms)*(Mo-Ms);
        U_standard = 10;
        
        if bootData(t,x*3+2) == 1 % 选择不公平选项
            U_delta = U - U_standard;
        else
            U_delta = U_standard - U;
        end
        
        prob_chosen(t,x) = 1/(1+exp(-lambda*U_delta));
    end
    
end

y = -sum(sum(log(prob_chosen)));



