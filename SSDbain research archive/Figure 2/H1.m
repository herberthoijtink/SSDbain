clear all
close all
marker={'*','o','+'};

figure('units','centimeters','Position',[5 5 8 6])
plot(NaN,NaN,'k*')
hold on
plot(NaN,NaN,'ko')
plot(NaN,NaN,'k+')
N=[10:20:300];
for j=1:3
med10=importdata(['bf_00_J',num2str(j),' .csv']);
med=med10.data;
hold on
plot(med,N,['k',marker{j}])
end
ylabel('Sample size N')
xlabel('Median BF')
grid on
for j=1:3
med10=importdata(['bf_00_J',num2str(j),' .csv']);
med=med10.data;
hold on
x=med.^2;
y=(N');
n=length(x);
b0=(sum(x.*y)-n*mean(x)*mean(y))/(sum(x.*x)-n*mean(x)^2);
b1=mean(y)-b0*mean(x);
y1=b0*x+b1;
plot(med,y1,'k-')
end
legend({'J=1','J=2','J=3'},'Location','Southeast')

figure('units','centimeters','Position',[5 5 8 6])
plot(NaN,NaN,'k*')
hold on
plot(NaN,NaN,'ko')
plot(NaN,NaN,'k+')
N=[400:20:600];
for j=1:3
med10=importdata(['bf_02_J',num2str(j),' .csv']);
med=med10.data;
hold on
plot((med),N,['k',marker{j}])
end
ylabel('Sample size N')
xlabel('Median BF')
grid on
for j=1:3
med10=importdata(['bf_02_J',num2str(j),' .csv']);
med=med10.data;
hold on
x=log(med);
y=(N');
n=length(x);
b0=(sum(x.*y)-n*mean(x)*mean(y))/(sum(x.*x)-n*mean(x)^2);
b1=mean(y)-b0*mean(x);
y1=b0*x+b1;
plot(exp(x),y1,'k-')
end
ylim([380,620])
legend({'J=1','J=2','J=3'},'Location','Southeast')
figure('units','centimeters','Position',[5 5 8 6])
plot(NaN,NaN,'k*')
hold on
plot(NaN,NaN,'ko')
plot(NaN,NaN,'k+')
N=[50:2:80];
for j=1:3
med10=importdata(['bf_05_J',num2str(j),' .csv']);
med=med10.data;
hold on
plot(med,N,['k',marker{j}])
end
ylabel('Sample size N')
xlabel('Median BF')
grid on
grid on
for j=1:3
med10=importdata(['bf_05_J',num2str(j),' .csv']);
med=med10.data;
hold on
x=log(med);
y=(N');
n=length(x);
b0=(sum(x.*y)-n*mean(x)*mean(y))/(sum(x.*x)-n*mean(x)^2);
b1=mean(y)-b0*mean(x);
y1=b0*x+b1;
plot(exp(x),y1,'k-')
end
ylim([45,85])
legend('J=1','J=2','J=3','Location','Southeast')

figure('units','centimeters','Position',[5 5 8 6])
plot(NaN,NaN,'k*')
hold on
plot(NaN,NaN,'ko')
plot(NaN,NaN,'k+')
N=[10:2:30];
for j=1:3
med10=importdata(['bf_08_J',num2str(j),' .csv']);
med=med10.data;
hold on
plot(med,N,['k',marker{j}])
end
ylabel('Sample size N')
xlabel('Median BF')
grid on
grid on
for j=1:3
med10=importdata(['bf_08_J',num2str(j),' .csv']);
med=med10.data;
hold on
x=log(med);
y=(N');
n=length(x);
b0=(sum(x.*y)-n*mean(x)*mean(y))/(sum(x.*x)-n*mean(x)^2);
b1=mean(y)-b0*mean(x);
y1=b0*x+b1;
plot(exp(x),y1,'k-')
end
legend('J=1','J=2','J=3','Location','Southeast')

figure('units','centimeters','Position',[5 5 8 6])
plot(NaN,NaN,'k*')
hold on
plot(NaN,NaN,'ko')
plot(NaN,NaN,'k+')
N=[10:2:30];
for j=1:3
med10=importdata(['bf_NULL_J',num2str(j),' .csv']);
med=med10.data;
hold on
plot(med,N,['k',marker{j}])
end
ylabel('Sample size N')
xlabel('Median BF')
grid on
grid on
for j=1:3
med10=importdata(['bf_NULL_J',num2str(j),' .csv']);
med=med10.data;
hold on
x=log(med);
y=(N');
n=length(x);
b0=(sum(x.*y)-n*mean(x)*mean(y))/(sum(x.*x)-n*mean(x)^2);
b1=mean(y)-b0*mean(x);
y1=b0*x+b1;
plot(exp(x),y1,'k-')
end
legend('J=1','J=2','J=3','Location','Southeast')
