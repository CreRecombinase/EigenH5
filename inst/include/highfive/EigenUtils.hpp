//
// Created by nwknoblauch on 12/20/17.
//

#pragma once


#ifdef H5_USE_EIGEN

#ifdef USE_BLOSC

#include <H5Ppublic.h>
#include <highfive/H5Filter.hpp>
#include "blosc_filter.h"

#endif

#include <highfive/H5File.hpp>
#include <highfive/H5DataSet.hpp>
#include <highfive/H5DataSpace.hpp>
#include <highfive/H5Group.hpp>
#include <highfive/H5Utility.hpp>

namespace HighFive {


    bool is_transposed(const DataSet &dataset) {
        int doTranspose = 0;
        if (dataset.hasAttribute("doTranspose")) {
            dataset.getAttribute("doTranspose").read(doTranspose);
        }
        return (doTranspose != 0);
    }

    template<typename Scalar, int RowsAtCompileTime, int ColsAtCompileTime, int Options>
    void read_mat_h5(
            const std::string &file_name,
            const std::string &group_path,
            const std::string &data_name,
            Eigen::Matrix<Scalar, RowsAtCompileTime, ColsAtCompileTime, Options> &retmat,
            std::vector<size_t> offsets = {0, 0},
            std::vector<size_t> chunk_size = {}) {
        HighFive::File file(file_name, HighFive::File::ReadOnly);

        auto group = file.getGroup(group_path);


        auto dataset = group.getDataSet(data_name);
        auto disk_dims = dataset.getDataDimensions();
        std::vector<size_t> disk_offset = {0, 0};

        if (chunk_size.empty()) {
            std::copy(disk_dims.begin(), disk_dims.end(), std::back_inserter(chunk_size));
            offsets = {0, 0};
        } else {
            disk_offset = offsets;
        }

        auto sel = dataset.selectEigen(offsets, chunk_size, std::vector<size_t>());
        sel.read(retmat);

    }


    template<typename Scalar, int RowsAtCompileTime, int ColsAtCompileTime, int Options>
    void write_mat_h5(
            const std::string &file_name,
            const std::string &group_path,
            const std::string &data_name,
            const Eigen::Matrix<Scalar, RowsAtCompileTime, ColsAtCompileTime, Options> &matrix,
            const bool doTranspose = false) {


        std::vector<Eigen::Index> map_dims(2, 0);
        assert(map_dims.size() == 2);


        HighFive::File file(file_name, HighFive::File::ReadWrite | HighFive::File::Create);


        Group group = file.createOrGetGroup(group_path);


        auto plist = H5Pcreate(H5P_DATASET_CREATE);

#ifdef USE_BLOSC
        int r = 0;
        r = register_blosc(nullptr, nullptr);

        // Create a new file using the default property lists.
        Filter filter({1000, 1000}, matrix, FILTER_BLOSC, doTranspose);
        // Create a dataset with double precision floating points
        plist = filter.getId();
#endif
        DataSpace ds = DataSpace::From(matrix, doTranspose);

        DataSet dataset = group.createDataSet(data_name, ds, AtomicType<Scalar>(), plist, doTranspose);
        dataset.write(matrix);
    }

    void write_vector_h5(const std::string &file_name,
                         const std::string &group_path,
                         const std::string &data_name,
                         const std::vector<std::string> &vector) {
        const bool doTranspose = false;
        std::vector<Eigen::Index> map_dims(2, 0);
        assert(map_dims.size() == 2);


        HighFive::File file(file_name, HighFive::File::ReadWrite | HighFive::File::Create);


        Group group = file.createOrGetGroup(group_path);


        auto plist = H5Pcreate(H5P_DATASET_CREATE);

#ifdef USE_BLOSC
        int r = 0;
        r = register_blosc(nullptr, nullptr);
        size_t chunksize = 1000;
        auto vsize = vector.size();
        if(vsize<=chunksize){
            chunksize=vsize;
        }
        auto cshape = {chunksize};
        // Create a new file using the default property lists.
        Filter filter(cshape, FILTER_BLOSC,r);
        // Create a dataset with double precision floating points
        plist = filter.getId();
#endif
        DataSpace ds = DataSpace::From(vector);

        DataSet dataset = group.createDataSet(data_name, ds, AtomicType<std::string>(), plist);
        dataset.write(vector);



    }
    template<typename Scalar>
    void write_vector_h5(
            const std::string &file_name,
            const std::string &group_path,
            const std::string &data_name,
            const Eigen::Matrix<Scalar,Eigen::Dynamic,1> &vector) {

        const bool doTranspose = false;
        std::vector<Eigen::Index> map_dims(2, 0);
        assert(map_dims.size() == 2);


        HighFive::File file(file_name, HighFive::File::ReadWrite | HighFive::File::Create);


        Group group = file.createOrGetGroup(group_path);


        auto plist = H5Pcreate(H5P_DATASET_CREATE);

#ifdef USE_BLOSC
        int r = 0;
        r = register_blosc(nullptr, nullptr);

        // Create a new file using the default property lists.
        Filter filter({1000}, vector, FILTER_BLOSC, doTranspose);
        // Create a dataset with double precision floating points
        plist = filter.getId();
#endif
        DataSpace ds = DataSpace::From(vector, doTranspose);

        DataSet dataset = group.createDataSet(data_name, ds, AtomicType<Scalar>(), plist, doTranspose);
        dataset.write(vector);
    }
    template<typename Scalar> void create_matrix_h5(const std::string &filename,const std::string &groupname,const std::string &dataname,const std::vector<size_t> &dimensions,const bool doTranspose=false){



        std::vector<size_t> mat_dims(dimensions.begin(),dimensions.end());
        //auto chunk_vec=reset_chunks_vec({100,100},mat_dims);
        std::vector<Scalar> mat_t(1);
        Eigen::Map<Eigen::Matrix<Scalar,Eigen::Dynamic,Eigen::Dynamic> > fake_mat(mat_t.data(),mat_dims[0],mat_dims[1]);
        File file(filename, File::ReadWrite |File::Create  );
        auto grp =file.createOrGetGroup(groupname);
        DataSpace ds = DataSpace::From(fake_mat, doTranspose);
        auto plist = H5Pcreate(H5P_DATASET_CREATE);

#ifdef USE_BLOSC
        int r = 0;
        r = register_blosc(nullptr, nullptr);
        // Create a new file using the default property lists.
        Filter filter({1000, 1000}, fake_mat, FILTER_BLOSC, doTranspose);
        // Create a dataset with double precision floating points
        plist = filter.getId();
#endif
        DataSet dataset = grp.createDataSet(dataname, ds, AtomicType<Scalar>(), plist, doTranspose);

    }

    template<typename Scalar> void create_vector_h5(const std::string &filename,const std::string &groupname,const std::string &dataname,const size_t length){

        std::vector<size_t> mat_dims={length};

        File file(filename, File::ReadWrite | File::Create);
        auto grp =file.createOrGetGroup(groupname);
        DataSpace ds = DataSpace({length});
        auto plist = H5Pcreate(H5P_DATASET_CREATE);

#ifdef USE_BLOSC
        int r = 0;
        r = register_blosc(nullptr, nullptr);
        // Create a new file using the default property lists.
        Filter filter({100},mat_dims,FILTER_BLOSC,r);
        // Create a dataset with double precision floating points
        plist = filter.getId();
#endif
        DataSet dataset = grp.createDataSet(dataname, ds, AtomicType<Scalar>(), plist);

    }
    template<typename Scalar, int RowsAtCompileTime, int ColsAtCompileTime, int Options>
    void write_mat_chunk_h5(
            const std::string &file_name,
            const std::string &group_path,
            const std::string &data_name,
            Eigen::Matrix<Scalar, RowsAtCompileTime, ColsAtCompileTime, Options> &retmat,
            std::vector<size_t> offsets = {0, 0},
            std::vector<size_t> chunk_size = {}) {

        HighFive::File file(file_name, HighFive::File::ReadWrite);

        auto group = file.getGroup(group_path);


        auto dataset = group.getDataSet(data_name);
        auto disk_dims = dataset.getDataDimensions();
        std::vector<size_t> disk_offset = {0, 0};

        if (chunk_size.empty()) {
            std::copy(disk_dims.begin(), disk_dims.end(), std::back_inserter(chunk_size));
            offsets = {0, 0};
        } else {
            disk_offset = offsets;
        }

        auto sel = dataset.selectEigen(offsets, chunk_size, std::vector<size_t>());
        sel.write(retmat);
    }




#endif
}


