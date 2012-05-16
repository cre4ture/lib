#ifndef SHARED_OBJECT_H
#define SHARED_OBJECT_H

#include <stdexcept>
#include <iostream>
#include <sys/types.h>

class shared_object_base
{
private:
    int m_shmid;
public:
    shared_object_base();
    virtual ~shared_object_base();
    void * operator new(unsigned int);
    void operator delete (void *p);
};

class shared_object_execute_base: public shared_object_base
{
private:
    pthread_mutex_t mutex_command_pending;
    pthread_mutex_t mutex_result_pending;
    bool terminate;
protected:
    virtual void execute_command() = 0;
public:
    shared_object_execute_base();
    ~shared_object_execute_base();
    virtual void startSlave();
    void start_execute(); // starts execute_command() in slave process
    void end_execute(); // waits till end of execute_command in slave process
};

template<class _objT>
class shared_func_call
{
public:
    // be called in slave
    virtual void execute_function(_objT* self) = 0;
};

template<class _objT, class _resultT>
class shared_func_call_result: shared_func_call<_objT>
{
protected:
    _resultT result;
public:
    // to be called in slave
    virtual void execute_function(_objT* self) = 0;
    _resultT getResult() { return result; }
};

template<class _objT, class _resultT, class _fpT>
class shared_func_call0: public shared_func_call_result<_objT, _resultT>
{
private:
    _resultT (_objT::*func_p)();

public:
    shared_func_call0(_resultT (_objT::*a_func_p)())
        : func_p(a_func_p) {}

    // to be called in slave
    void execute_function(_objT* self)
    {
        this->result = (self->*func_p)();
    }
};

template<class _objT, class _resultT, class _fpT, class _pT1>
class shared_func_call1: public shared_func_call_result<_objT, _resultT>
{
private:
    _resultT (_objT::*func_p)(_pT1);
    _pT1 param1;

public:
    shared_func_call1(_resultT (_objT::*a_func_p)(_pT1), _pT1 a_param1)
        : func_p(a_func_p), param1(a_param1) {}

    // be called in slave
    void execute_function(_objT* self)
    {
        this->result = (self->*func_p)(param1);
    }
};

template<class _objT, class _resultT, class _fpT, class _pT1, class _pT2>
class shared_func_call2: public shared_func_call_result<_objT, _resultT>
{
private:
    _resultT (_objT::*func_p)(_pT1, _pT2);
    _pT1 param1;
    _pT2 param2;

public:
    shared_func_call2(_resultT (_objT::*a_func_p)(_pT1, _pT2), _pT1 a_param1, _pT2 a_param2)
        : func_p(a_func_p), param1(a_param1), param2(a_param2) {}

    // to be called in slave
    void execute_function(_objT* self)
    {
        this->result = (self->*func_p)(param1, param2);
    }
};

template<class _parentT, bool _is_shared, size_t _param_buffer_size = 1024>
class shared_object: private _parentT, // this inheritence is forces private cause noone should call memberfunctions directly! Use call_function_X instead!
                     public shared_object_execute_base
{
private:
    typedef shared_object<_parentT, _is_shared, _param_buffer_size> thisType;
    char data_buffer[_param_buffer_size];

    void execute_command()
    {
        shared_func_call<thisType>* call = (shared_func_call<thisType>*)data_buffer;
        call->execute_function(this);
    }

public:
    shared_object() {}

    void startSlave()
    {
        if (_is_shared)
        {
            shared_object_execute_base::startSlave();
        }
    }

    template <class _resultT, class _fpT>
    void start_function(_fpT func)
    {
        typedef shared_func_call0<thisType, _resultT, _fpT> my_call;
        // check size
        if (sizeof(my_call) > _param_buffer_size)
        { throw std::runtime_error("shared_object::start_function(): Sizeof function call is to big!"); }
        // creates function call object
        my_call* call = new (data_buffer) my_call(func);
        // let slave work:
        start_execute();
    }

    template <class _resultT, class _p1T, class _fpT>
    void start_function(_fpT func, _p1T param1)
    {
        typedef shared_func_call1<thisType, _resultT, _fpT, _p1T> my_call;
        // check size
        if (sizeof(my_call) > _param_buffer_size)
        { throw std::runtime_error("shared_object::start_function(): Sizeof function call is to big!"); }
        // creates function call object
        my_call* call = new (data_buffer) my_call(func, param1);
        // let slave work:
        start_execute();
    }

    template <typename _resultT, class _p1T, class _p2T, typename _fpT>
    void start_function(_fpT func, _p1T param1, _p2T param2)
    {
        typedef shared_func_call2<thisType, _resultT, _fpT, _p1T, _p2T> my_call;
        // check size
        if (sizeof(my_call) > _param_buffer_size)
        { throw std::runtime_error("shared_object::start_function(): Sizeof function call is to big!"); }
        // creates function call object
        my_call* call = new (data_buffer) my_call(func, param1, param2);
        // let slave work:
        start_execute();
    }

    template <typename _resultT>
    _resultT end_function()
    {
        // wait to be executed
        end_execute();
        shared_func_call_result<thisType, _resultT>* call = (shared_func_call_result<thisType, _resultT>*)data_buffer;
        return call->getResult();
    }

    template <typename _resultT, typename _fpT>
    _resultT call_function(_fpT func)
    {
        if (_is_shared)
        {
            start_function<_resultT, _fpT>(func);
            return end_function<_resultT>();
        }
        else
        {
            // not shared: simply call function
            return (this->*func)();
        }
    }

    template <class _resultT, class _p1T, class _fpT>
    _resultT call_function(_fpT func, _p1T param1)
    {
        if (_is_shared)
        {
            start_function<_resultT, _p1T, _fpT>(func, param1);
            return end_function<_resultT>();
        }
        else
        {
            // not shared: simply call function
            return (this->*func)(param1);
        }
    }

    template <class _resultT, class _p1T, class _p2T, class _fpT>
    _resultT call_function(_fpT func, _p1T param1, _p2T param2)
    {
        if (_is_shared)
        {
            start_function<_resultT, _p1T, _p2T, _fpT>(func, param1, param2);
            return end_function<_resultT>();
        }
        else
        {
            // not shared: simply call function
            return (this->*func)(param1, param2);
        }
    }
};

template<class _parentT, bool _is_shared, size_t _param_buffer_size = 1024>
class extendable_shared_object: public shared_object<_parentT, _is_shared, _param_buffer_size>
{
private:

public:

};

#endif // SHARED_OBJECT_H
