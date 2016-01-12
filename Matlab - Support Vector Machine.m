    
%---------------------------------------------%
% Voy a correr esto en un ciclo, cambiando de
% base de datos sobre la que corro el código.
%
% el numerador s es el archivo
% ronda es para intercambiar test y train
%
% a1a-a9a tienen el nombre original de LIBSV.
% gisette_scale cambia a a10a
% ijcnn1 cambia a a11a
% kdda cambia a a200a
% kddb cambia a a300a
% leukemia cambia a a12a
%
% los w1a los cambie a a13a hasta a20a
 
pct=[0.01; 0.02; 0.05; 0.1];
 

 
for y=2:3
    for s=10:10 
        for ronda=1:1
            
            porcentaje=pct(y);
                
            [trainY, trainX]=libsvmread('LIBSVM_train_3.txt');
            [testY, testX]=libsvmread('LIBSVM_test_3.txt');            
            
 
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%        MODELO ENTERO         %%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 
            %%%% AQUÍ ENCUENTRO LOS MEJORES PARÁMETROS C Y GAMMA
            [obs,c]=size(trainY);
            perm=(randperm(obs));
 
            porc=round(porcentaje*obs);
 
            min_trainX=trainX(perm(1:porc),:);
            min_trainY=trainY(perm(1:porc),:);
 
            bestcv = 0;
            for log2c = -1:3,
              for log2g = -4:1,
                cmd = ['-v 5 -c ', num2str(2^log2c), ' -g ', num2str(2^log2g)];
                cv = svmtrain(min_trainY,min_trainX, cmd);
                if (cv >= bestcv),
                  bestcv = cv; 
                  bestc = 2^log2c; 
                  bestg = 2^log2g;
                end
                fprintf('%g %g %g (best c=%g, g=%g, rate=%g)\n', log2c, log2g, cv, bestc, bestg, bestcv);
              end
            end
 
                inicio=clock;
                cmd = ['-t 2 -c ', num2str(bestc),' -g ', num2str(bestg)];
                model= svmtrain(trainY,trainX, cmd);
                [predicted_label, accuracy, decision_values] = svmpredict(testY, testX, model);
                fin=clock;            
 
                VECTORES_ORIG=full(model.SVs);
                
                a=size(trainY);
 
                todo(1,1,1)=0;
                todo(1,2,1)=(fin(4)-inicio(4))*60+(fin(5)-inicio(5))+(fin(6)-inicio(6))/60;
                todo(1,3,1)=accuracy(1);
                todo(1,4,1)=model.totalSV;
                todo(1,5,1)=model.nSV(1);
                todo(1,6,1)=model.nSV(2);
                todo(1,7,1)=a(1);
 
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
            % AQUI COJO OBSERVACIONES ALEATORIAS DE LA MUESTRA DE ENTRENAMIENTO
            % ESTA ES UNA BASE DE DATOS GRANDE QUE BAJÉ DE INTERNET
            % ESTE EJERCICIO ES 1 VECTOR MÁS CERCANO, CON CIEN DE PARTIDA
 
            for j=1:8;
                trainYsub=trainY(perm(1:porc),:);
                trainXsub=trainX(perm(1:porc),:);
 
                trainYsub2=trainYsub;
                trainXsub2=trainXsub;
 
                a=size(trainYsub2);
 
                ini=clock;
                cmd = ['-t 2 -c ', num2str(bestc),' -g ', num2str(bestg)];
                model= svmtrain(trainYsub2,trainXsub2, cmd);
                [predicted_label, accuracy, decision_values] = svmpredict(testY, testX, model);
                fi=clock;
 
                todo(2,1,j)=1;
                todo(2,2,j)=(fi(4)-ini(4))*60+(fi(5)-ini(5))+(fi(6)-ini(6))/60;
                todo(2,3,j)=accuracy(1);
                todo(2,4,j)=model.totalSV;
                todo(2,5,j)=model.nSV(1);
                todo(2,6,j)=model.nSV(2);
                todo(2,7,j)=a(1);
                todo(2,8,j)=todo(2,2,j);
 
                for i=1:5
 
                    [tmp index]=ismember(model.SVs, trainX,'rows');
 
                    ini=clock;
                    imp=unique(kcercanos(trainX(index,:),trainX,j));
 
                    trainYsub2=[trainY(index,:);trainY(imp,:)];
                    trainXsub2=[trainX(index,:);trainX(imp,:)];   
 
                    a=size(trainYsub2);
                    cmd = ['-t 2 -c ', num2str(bestc),' -g ', num2str(bestg)];
                    model= svmtrain(trainYsub2,trainXsub2, cmd);
                    [predicted_label, accuracy, decision_values] = svmpredict(testY, testX, model);
                    fi=clock;
 
                    VECTORES_2=full(model.SVs);
                    
                    incluidos=ismember(VECTORES_2, VECTORES_ORIG,'rows');
                    
                    [total_VS_origi,xx]=size(VECTORES_ORIG);
                    [total_VS_iterac,xxx]=size(incluidos);
                    pct_VS=total_VS_iterac/total_VS_origi;
                    
                    todo(i+2,1,j)=i;
                    todo(i+2,2,j)=(fi(4)-ini(4))*60+(fi(5)-ini(5))+(fi(6)-ini(6))/60;
                    todo(i+2,3,j)=accuracy(1);
                    todo(i+2,4,j)=model.totalSV;
                    todo(i+2,5,j)=model.nSV(1);
                    todo(i+2,6,j)=model.nSV(2);
                    todo(i+2,7,j)=a(1);
                    todo(i+2,8,j)=todo(i+2,2,j)+todo(i+1,8,j);
                    todo(i+2,9,j)=pct_VS;
                    
                end
            end 
 
            nombre_excel= ['porcent_', num2str(pct(y))];
            
            xlswrite(nombre_excel,todo(:,:,1),(s-10)*2+ronda,'A1:I7');
            xlswrite(nombre_excel,todo(:,:,2),(s-10)*2+ronda,'A9:I15');
            xlswrite(nombre_excel,todo(:,:,3),(s-10)*2+ronda,'A17:I23');
            xlswrite(nombre_excel,todo(:,:,4),(s-10)*2+ronda,'A25:I31');
            xlswrite(nombre_excel,todo(:,:,5),(s-10)*2+ronda,'A33:I39');
            xlswrite(nombre_excel,todo(:,:,6),(s-10)*2+ronda,'A41:I47');
            xlswrite(nombre_excel,todo(:,:,7),(s-10)*2+ronda,'A49:I55');
 
        end
    end
end

