function [Value, Policya, Policyc]=vfi_finite()
% This function solves the consumer problem of a finitely-lived agent, that
% lives during T periods, has possible income realizations given by
% 'ygrid', that follow a Markov(1) process. The variable 'det' is equal to
% one if there is a deterministic trend on the income process, given by the
% vector 'param.y_det', where y_t=y_det*ygrid. After retirement (age Jret),
% the individual earns a constant amount equal to y_det(Jret)*theta.
% If mort==1, there is a vector of survival probabilities in param.death.

global par

% Initialization
Value   = zeros(par.T+1,numel(par.hgrid),numel(par.egrid),numel(par.bgrid));
Policye = zeros(par.T+1,numel(par.hgrid),numel(par.egrid),numel(par.bgrid));
Policyb = zeros(par.T+1,numel(par.hgrid),numel(par.egrid),numel(par.bgrid));
Policyc = zeros(par.T+1,numel(par.hgrid),numel(par.egrid),numel(par.bgrid));
Policyd = zeros(par.T+1,numel(par.hgrid),numel(par.egrid),numel(par.bgrid));

% State variables:   health (h), exercise (e), bonds (b)
% Control varuables: exercise (ep), bonds (bp), doctor expenses (d)
[h, e, b, ep, bp, d]=ndgrid(par.hgrid, par.egrid, par.bgrid, par.egrid, par.bgrid, par.dgrid);

for age = T+1:-1:1
    if age < T+1

        I       = ones(par.nh, par.ne, par.nb, par.ne, par.nb, par.nd);  % Auxiliar matrix of ones
        
        c       = ((1+par.r)*b+par.w-bp-d).*((1+par.r)*b+par.w-bp-d>0) + eps*((1+par.r)*b+par.w-bp-d<=0);
        sick_t  = sick(h);
        l       = (I - ep - sick_t).*(I - ep - sick_t > 0) + eps.*(I - ep - sick_t <= 0);
        max_tem = (I - sick_t).*(I - sick_t > 0) + eps.*(I - sick_t <= 0);
        
        hp_h    = (1-delta_dep(age))*h + invest(d, ep);
        hp_l    = (1-delta_dep(age))*h + invest(d, ep) - par.eps_l;
        
        for ie = 1:par.ne
            for ib = 1:par.nb
                Vex(:, ie, ib) = intv(Value(age+1, :, ie, ib), par.hgrid, hp_h)
            end
        end

        if sigma == 1
            V   = log(c).*((1+par.r)*b+par.w-bp-d>0)-10^20*((1+par.r)*b+par.w-bp-d<=0)+...
                (I+sigma_add(e)).*par.gamma.*(l.^(1+par.eta))./(1+par.eta) - ...
                sigma_add(e).*par.gamma.*(max_tem.^(1+par.eta))./(1+par.eta)
        
                pi_prob(h,age)*beta*
        
        
                        mort_prob(age)*beta.*repmat((P*permute(Value(age+1, :, :),[2 3 1])), [1, 1, numel(agrid)]);
        else
            V           = (c.^(1-sigma)-1)/(1-sigma).*(w*yp_act+(1+r)*ap-aprime>0)-10^20*(w*yp_act+(1+r).*ap-aprime<=0)+...
                        mort_prob(age)*beta.*repmat((P*permute(Value(age+1, :, :),[2 3 1])), [1, 1, numel(agrid)]);
        end
                
        [Val, Pol] = max(V,[],2);
        Value(age, :, :)    = permute(Val,[2,1,3]);
        Policya(age, :, :)  = permute(Pol,[2,1,3]);
        
        for i=1:numel(ygrid)
            for j=1:numel(agrid)
                Policyc(age, i, j) = c(i, Policya(age, i,j), j);
            end
        end
        
    else
        % In last period, there are no doctor expenditures, exercise is
        % equal to zero, and savings are equal to zero.
        Policyc(age,:,:,:)  = (1+par.r)*b(:,:,:,1,1)+par.w;
        if sigma == 1
            Value(age,:,:,:)    = log(Policyc(age,:,:,:)) + (I - sick_t).*(I - sick_t > 0) + eps.*(I - sick_t <= 0);
        else
            Value(age,:,:,:)    = (Policyc(age,:,:,:).^(1-sigma)-1)./(1-sigma);
        end
        Policyb(age,:,:,:)  = find((par.bgrid==0));
    end
    
    for ia = 1:numel(agrid)
        for iy = 1:numel(ygrid)
            if Policyc(age, iy, ia) <= 0
                Value(age, iy, ia) = -inf;
            end
        end
    end
end

end