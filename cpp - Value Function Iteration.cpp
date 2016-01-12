// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <vector>
using namespace Rcpp;
using namespace arma;
using std::vector;


/*** Probability of surviving to next period ***/
double pi_prob(double health, int t, double zeta, double omega, int T) {
  
  double probab = 0;
  
  if(health >= 0 && t <= T){
    probab = (1-exp(-zeta* pow(health, omega)));
  }
  
  return probab;
  
}

/*** Health depreciation ***/
double delta_dep(int t, double a0, double a1, double a2, double depre) {
  
  double x = exp(a0 + a1*t + a2*pow(t,2)) / (1 + exp(a0 + a1*t + a2*pow(t,2))); 

  return x;
  
}

/*** Investment function in health ***/
double invest(double doctor, double exercise, double rho, double kappa, double iota, double Omega, double prod_e) {
  
  double invest = 0;
  
  if(iota == 1.0){
    invest = Omega * pow((pow(doctor,rho)*pow((prod_e*exercise) ,1-rho)),kappa);
  }
  else{
    invest = Omega * pow(rho*pow(doctor,((iota-1)/iota))+(1-rho)*pow((prod_e*exercise),((iota-1)/iota)),((iota*kappa)/(iota-1)));
  }
  
  return invest;
  
}


/*** Sick time ***/
double sick(double Q, double psi, double health) {
  
  double x = Q * pow(health, -psi);
  
  if (x>0.999){
    x = 0.999;
  }
  
  return x;
  
}


/*** Probability of bad health shock ***/
double prob_shock(int t, vector<double> probs) {
  
  double x = 0;
  
  if(t<40){
    x = probs[0];
  }
  else{
    x = probs[t-40];
  }
  return x;
  
}


/*** Absolute value of elements of a vector ***/
vector<double> abs(vector<double> a) {
  
  vector<double> x = a;
  int siz = a.size();
  
  for(int i=0; i<siz; i++){
    if(x[i]<0){
      x[i] = -a[i];
    }
  }
  
  return x;
  
}

/*** Minimum entry of a vector ***/
int min(vector<double> V) {
  
  int siz = V.size();
  int min = 0;
  
  for(int i=0; i<siz; i++){
    if(V[i]<V[min]){
      min = i;
    }
  }
  return min;
  
}

/*** Interpolation ***/
vector<double> interp1(vector<double> Y, vector<double> V, vector<double> Gy) {
  
  int si = Gy.size();
  int siY = Y.size();
  double t;
  
  vector<double> IV (si);
  vector<double> diff (siY);
  vector<double> aux (siY);
  double xmin;

  for(int i=0; i < si; i++){
    for(int j=0; j < siY; j++){
      diff[j] = Y[j] - Gy[i];
    }
    
    aux = abs(diff);    
    xmin = min(aux);
    
    if(Gy[i]<Y[xmin]){
      xmin = xmin-1;
    }
    if(xmin<0){
      xmin = 0;
    }
    if(xmin>(siY-2)){
      xmin = siY-2;
    }
    
    t = (V[(xmin+1)]-V[xmin]) / (Y[(xmin+1)]-Y[xmin]);
    IV[i] = t*(Gy[i] - Y[xmin]) + V[xmin]; 
  }
  
  return IV;
  
}

/*** THE FUNCTION ***/
// [[Rcpp::export]]
vector<vector<vector<vector<double> > > >  vfi(vector<double> hgrid, vector<double> egrid, vector<double> bgrid, vector<double> dgrid, int T, int nh, int ne, int nb, int nd, vector<double> w, double r, double P_d, double theta, double gamma, double eta, double xi, double lambda, double beta, double Q, double psi, double a0, double a1, double a2, double depre, double zeta, double omega, double rho, double kappa, double iota, double Omega, double nu, double eps_l, double eps_m, int impatience, double p_high, vector<double> probs, double prod_e, int reduction, double morfina, double health_limit, double morf_mult, double cost_e, double expo_e) {  
  /*** Initializing ***/
  
  vector<double> res (4);
    
  vector<vector<vector<vector<double> > > > All_matrices;
  vector<vector<vector<double> > > Value;
  vector<vector<vector<double> > > Policyb;
  vector<vector<vector<double> > > Policyc;
  vector<vector<vector<double> > > Policyd;
  vector<vector<vector<double> > > Policye;
  vector<vector<vector<double> > > Policyl;
  vector<vector<vector<double> > > Policym;
  
  All_matrices.resize(T);
  Value.resize(T);
  Policyb.resize(T);
  Policyc.resize(T);
  Policyd.resize(T);
  Policye.resize(T);
  Policyl.resize(T);
  Policym.resize(T);
  
  for(int i=0; i<T; i++){
    All_matrices[i].resize(nh);
    Value[i].resize(nh);
    Policyb[i].resize(nh);
    Policyc[i].resize(nh);
    Policyd[i].resize(nh);
    Policye[i].resize(nh);
    Policyl[i].resize(nh);
    Policym[i].resize(nh);
    
    for(int j=0; j<nh; j++){
      All_matrices[i][j].resize(nb);
      Value[i][j].resize(nb);
      Policyb[i][j].resize(nb);
      Policyc[i][j].resize(nb);
      Policyd[i][j].resize(nb);
      Policye[i][j].resize(nb);
      Policyl[i][j].resize(nb);
      Policym[i][j].resize(nb);
      
        for(int l=0; l<nb; l++){
          Value[i][j][l] = pow(-10,25);
          Policyc[i][j][l] = pow(10,-20);
          Policyl[i][j][l] = 0;
          Policym[i][j][l] = 0;
        }
        for(int l=0; l<nb; l++){
          All_matrices[i][j][l].resize(7);
        }
      }
    }
  
  vector<double> hp_h (1);
  vector<double> hp_m (1);
  vector<double> hp_l (1);
  vector<double> Vaux (nh);
  vector<double> Vexl (1);
  vector<double> Vexm (1);
  vector<double> Vexh (1);
  vector<double> Vex (1);
  
  /*** Backwards induction ***/
  
  for(int age=T-1; age>=0; age--){

  double c = pow(10,-20);
  double l = 0.0;
  double sick_t = 0.0;
  double VV = pow(-10,25);
  double c_aux1 = pow(10,-20);
  double c_aux2 = pow(10,-20);
  double V_aux1 = pow(-10,25);
  double V_aux2 = pow(-10,25);

    /*** Terminal period ***/
    if(age == T-1){
      
      for(int ih=0; ih<nh; ih++){
        for(int ib=0; ib<nb; ib++){

          /*** Morfina vs no morfina ***/
          if(hgrid[ih] >= health_limit){
            
            if((1+r)*bgrid[ib] + w[age] > 0){
              Policyc[age][ih][ib] = (1+r)*bgrid[ib] + w[age];
            }
            
            if(1 - sick(Q, psi, hgrid[ih]) >=0 ){
              Policyl[age][ih][ib] = 1 - sick(Q, psi, hgrid[ih]);
            }

            if(((1+r)*bgrid[ib] + w[age] > 0) && (1 - sick(Q, psi, hgrid[ih]) >=0) ){
              if(theta != 1){
                Value[age][ih][ib] = (pow((lambda*pow(Policyc[age][ih][ib],1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi)),((1-theta)/(1-xi))))/(1-theta) + gamma * pow(Policyl[age][ih][ib], 1+eta)/(1+eta);
              }
              else{
                /*** As in He and Huang ***/
                Value[age][ih][ib] = log((lambda*pow(Policyc[age][ih][ib],1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi)))/(1-xi) + gamma * log(Policyl[age][ih][ib]);
              }
            }
          }
          else{
            if((1+r)*bgrid[ib] + w[age] > 0){
              c_aux1 = (1+r)*bgrid[ib] + w[age];
            }
            if((1+r)*bgrid[ib] + w[age] - morfina > 0){
              c_aux2 = (1+r)*bgrid[ib] + w[age] - morfina;
            }
            
            if(1 - sick(Q, psi, hgrid[ih]) >=0 ){
              Policyl[age][ih][ib] = 1 - sick(Q, psi, hgrid[ih]);
            }
            if(((1+r)*bgrid[ib] + w[age] > 0) && (1 - sick(Q, psi, hgrid[ih]) >=0) ){
              if(theta != 1){
                V_aux1 = (pow((lambda*pow(c_aux1,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi)),((1-theta)/(1-xi))))/(1-theta) + gamma * pow(Policyl[age][ih][ib], 1+eta)/(1+eta);
              }
              else{
                /*** As in He and Huang ***/
                V_aux1 = log((lambda*pow(c_aux1,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi)))/(1-xi) + gamma * log(Policyl[age][ih][ib]);
              }
            }
            if(((1+r)*bgrid[ib] + w[age] - morfina > 0) && (1 - sick(Q, psi, hgrid[ih]) >=0) ){
              if(theta != 1){
                V_aux2 = (pow((lambda*pow(c_aux2,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi)),((1-theta)/(1-xi))))/(1-theta) + morfina*morf_mult + gamma * pow(Policyl[age][ih][ib], 1+eta)/(1+eta);
              }
              else{
                /*** As in He and Huang ***/
                V_aux2 = log((lambda*pow(c_aux2,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi)))/(1-xi) + morfina*morf_mult + gamma * log(Policyl[age][ih][ib]);
              }
            }
            if(V_aux1 >= V_aux2){
              Value[age][ih][ib] = V_aux1;
              Policyc[age][ih][ib] = c_aux1;
            }
            else{
              Value[age][ih][ib] = V_aux2;
              Policyc[age][ih][ib] = c_aux2;
              Policym[age][ih][ib] = morfina;
            }
          }

          
          All_matrices[age][ih][ib][0] = Value[age][ih][ib];
          All_matrices[age][ih][ib][1] = Policyb[age][ih][ib];
          All_matrices[age][ih][ib][2] = Policyc[age][ih][ib];
          All_matrices[age][ih][ib][3] = Policyd[age][ih][ib];
          All_matrices[age][ih][ib][4] = Policye[age][ih][ib];
          All_matrices[age][ih][ib][5] = Policyl[age][ih][ib];
          All_matrices[age][ih][ib][6] = Policym[age][ih][ib];
  
        }
      
      }
    }
    
    
    /*** Other periods ***/ 
    
    if(age < T-1){

      Rcout << "Age is:" << age+1 << std::endl;

      for(int ih=0; ih<nh; ih++){
        for(int ib=0; ib<nb; ib++){
          for(int iep=0; iep<ne; iep++){
            for(int ibp=0; ibp<nb; ibp++){
              for(int idp=0; idp<nd; idp++){
                 
                 if(hgrid[ih] >= health_limit){
                  if((1+r)*bgrid[ib] + w[age] - bgrid[ibp] - P_d * dgrid[idp] > 0){
                    c = (1+r)*bgrid[ib] + w[age] - bgrid[ibp] - P_d * dgrid[idp];
                  }
                  
                  sick_t = sick(Q, psi, hgrid[ih]);
                  if(1 - sick_t - egrid[iep]>=0){
                    l = 1 - sick_t - egrid[iep];
                  }
                  
                  hp_h[0] = (1 - delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih] + invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e);
                  hp_m[0] = (1 - delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih] + invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e) - eps_m;
                  hp_l[0] = (1 - delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih] + invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e) - eps_l;
                  
                  for(int i=0; i<nh; i++){
                    Vaux[i] = Value[age+1][i][ibp];
                  }
                  
                  Vexl = interp1(hgrid, Vaux, hp_l);
                  Vexm = interp1(hgrid, Vaux, hp_m);
                  Vexh = interp1(hgrid, Vaux, hp_h);
                  
                  Vex[0] = p_high*(prob_shock(age+1, probs)*Vexl[0] + (1-prob_shock(age+1, probs))*Vexh[0]) + (1-p_high)*(prob_shock(age+1, probs)*Vexm[0] + (1-prob_shock(age+1, probs))*Vexh[0]);
                  
                  if( ( (1+r)*bgrid[ib] + w[age] - bgrid[ibp] - P_d * dgrid[idp] > 0) && (1 - sick_t - egrid[iep]>=0) ){
                    
                    if(theta != 1){
                      VV = (pow(lambda*pow(c,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi),((1-theta)/(1-xi))))/(1-theta) + gamma*pow(l,1+eta)/(1+eta) - cost_e*pow(egrid[iep], (1+expo_e))/(1+expo_e) + beta * pow(pi_prob(hgrid[ih], age+1, zeta, omega, T),reduction) * Vex[0];
                    }
                    else{
                      VV = log(lambda*pow(c,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi))/(1-xi)                         + gamma*log(l)               - cost_e*pow(egrid[iep], (1+expo_e))/(1+expo_e) + beta * pow(pi_prob(hgrid[ih], age+1, zeta, omega, T),reduction) * Vex[0];
                    }

                  }
                  /*** The maximum ***/
                  if(VV > Value[age][ih][ib]){
                    Value[age][ih][ib] = VV;
                    Policyc[age][ih][ib] = c;
                    Policyl[age][ih][ib] = l;
                    Policyb[age][ih][ib] = bgrid[ibp];
                    Policyd[age][ih][ib] = dgrid[idp];
                    Policye[age][ih][ib] = egrid[iep];
                    
                    All_matrices[age][ih][ib][0] = Value[age][ih][ib];
                    All_matrices[age][ih][ib][1] = Policyb[age][ih][ib];
                    All_matrices[age][ih][ib][2] = Policyc[age][ih][ib];
                    All_matrices[age][ih][ib][3] = Policyd[age][ih][ib];
                    All_matrices[age][ih][ib][4] = Policye[age][ih][ib];
                    All_matrices[age][ih][ib][5] = Policyl[age][ih][ib];
                    All_matrices[age][ih][ib][6] = Policym[age][ih][ib];
                  }
                }
                else{
                  for(double mor=0; mor<2; mor++){
                    if((1+r)*bgrid[ib] + w[age] - bgrid[ibp] - P_d * dgrid[idp] - mor*morfina > 0){
                      c = (1+r)*bgrid[ib] + w[age] - bgrid[ibp] - P_d * dgrid[idp] - mor*morfina;
                    }
                    
                    sick_t = sick(Q, psi, hgrid[ih]);
                    if(1 - sick_t - egrid[iep]>=0){
                      l = 1 - sick_t - egrid[iep];
                    }
                    
                    hp_h[0] = (1 - delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih] + invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e);
                    hp_m[0] = (1 - delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih] + invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e) - eps_m;
                    hp_l[0] = (1 - delta_dep((age+1), a0, a1, a2, depre))*hgrid[ih] + invest(dgrid[idp], egrid[iep], rho, kappa, iota, Omega, prod_e) - eps_l;
                    
                    for(int i=0; i<nh; i++){
                      Vaux[i] = Value[age+1][i][ibp];
                    }
                    
                    Vexl = interp1(hgrid, Vaux, hp_l);
                    Vexm = interp1(hgrid, Vaux, hp_m);
                    Vexh = interp1(hgrid, Vaux, hp_h);
                    
                    Vex[0] = p_high*(prob_shock(age+1, probs)*Vexl[0] + (1-prob_shock(age+1, probs))*Vexh[0]) + (1-p_high)*(prob_shock(age+1, probs)*Vexm[0] + (1-prob_shock(age+1, probs))*Vexh[0]);
                    
                    if( ( (1+r)*bgrid[ib] + w[age] - bgrid[ibp] - P_d * dgrid[idp]- mor*morfina > 0) && (1 - sick_t - egrid[iep]>=0) ){
                      if(theta != 1){
                        VV = (pow(lambda*pow(c,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi),((1-theta)/(1-xi))))/(1-theta) + mor*morfina*morf_mult + gamma*pow(l,1+eta)/(1+eta) - cost_e*pow(egrid[iep], (1+expo_e))/(1+expo_e) + beta * pow(pi_prob(hgrid[ih], age+1, zeta, omega, T),reduction) * Vex[0];
                      }
                      else{
                        VV = log(lambda*pow(c,1-xi) + (1-lambda)*pow(hgrid[ih], 1-xi))/(1-xi)                         + mor*morfina*morf_mult + gamma*log(l)               - cost_e*pow(egrid[iep], (1+expo_e))/(1+expo_e) + beta * pow(pi_prob(hgrid[ih], age+1, zeta, omega, T),reduction) * Vex[0];
                      }
                    }
                    /*** The maximum ***/
                    if(VV > Value[age][ih][ib]){
                      Value[age][ih][ib] = VV;
                      Policyc[age][ih][ib] = c;
                      Policyl[age][ih][ib] = l;
                      Policyb[age][ih][ib] = bgrid[ibp];
                      Policyd[age][ih][ib] = dgrid[idp];
                      Policye[age][ih][ib] = egrid[iep];
                      Policym[age][ih][ib] = mor*morfina;
                      
                      All_matrices[age][ih][ib][0] = Value[age][ih][ib];
                      All_matrices[age][ih][ib][1] = Policyb[age][ih][ib];
                      All_matrices[age][ih][ib][2] = Policyc[age][ih][ib];
                      All_matrices[age][ih][ib][3] = Policyd[age][ih][ib];
                      All_matrices[age][ih][ib][4] = Policye[age][ih][ib];
                      All_matrices[age][ih][ib][5] = Policyl[age][ih][ib];
                      All_matrices[age][ih][ib][6] = Policym[age][ih][ib];
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  
  
  
  return All_matrices;
}









